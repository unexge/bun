const std = @import("std");

const bun = @import("root").bun;
const string = bun.string;

/// A Scanner for Yarn lockfile v1.
pub const Scanner = struct {
    pub const Token = union(enum) {
        at,
        colon,
        comma,
        space,

        comment: []const u8,
        literal: []const u8,
        string: []const u8,

        end_of_file,
    };

    source: []const u8,
    pos: usize = 0,
    state: union(enum) {
        start,
        comment: usize,
        literal: usize,
        string: usize,
    } = .start,
    head: ?Token = null,

    pub fn init(source: []const u8) Scanner {
        return Scanner{ .source = source };
    }

    pub fn peek(self: *Scanner) Token {
        if (self.head) |head| {
            return head;
        }

        self.head = self.next();
        return self.head.?;
    }

    pub fn next(self: *Scanner) Token {
        if (self.head) |head| {
            self.head = null;
            return head;
        }

        while (self.pos < self.source.len) {
            const c = self.source[self.pos];
            switch (self.state) {
                .start => {
                    switch (c) {
                        '#' => {
                            self.state = .{ .comment = self.pos };
                            self.pos += 1;
                        },
                        ' ' => {
                            self.pos += 1;
                            return .space;
                        },
                        '@' => {
                            self.pos += 1;
                            return .at;
                        },
                        ':' => {
                            self.pos += 1;
                            return .colon;
                        },
                        ',' => {
                            self.pos += 1;
                            return .comma;
                        },
                        '"' => {
                            self.state = .{ .string = self.pos };
                            self.pos += 1;
                        },
                        '\n', '\t' => {
                            self.pos += 1;
                        },
                        else => {
                            self.state = .{ .literal = self.pos };
                            self.pos += 1;
                        },
                    }
                },
                .comment => |start| switch (c) {
                    '\n' => {
                        const comment = self.source[start..self.pos];
                        self.state = .start;
                        self.pos += 1;
                        return .{ .comment = comment };
                    },
                    else => {
                        self.pos += 1;
                    },
                },
                .literal => |start| switch (c) {
                    ',', ':', ' ', '@', '\n' => {
                        const literal = self.source[start..self.pos];
                        self.state = .start;
                        return .{ .literal = literal };
                    },
                    else => {
                        self.pos += 1;
                    },
                },
                .string => |start| switch (c) {
                    '"' => {
                        self.pos += 1;
                        const str = self.source[start..self.pos];
                        self.state = .start;
                        return .{ .string = str };
                    },
                    else => {
                        self.pos += 1;
                    },
                },
            }
        }

        return .end_of_file;
    }
};

/// A Parser for Yarn lockfile v1.
pub const Parser = struct {
    arena: std.heap.ArenaAllocator,
    scanner: Scanner,

    pub const Entry = union(enum) {
        comment: string,
        block: Block,
    };

    pub const Block = struct {
        packages: std.ArrayList(Block.Package),
        version: string,
        resolved: string,
        integrity: ?string = null,
        dependencies: std.ArrayList(Block.Package),
        optionalDependencies: std.ArrayList(Block.Package),

        pub const Package = struct {
            name: string,
            version: string,
        };
    };

    pub fn init(alloc: std.mem.Allocator, source: []const u8) Parser {
        return Parser{ .arena = std.heap.ArenaAllocator.init(alloc), .scanner = Scanner.init(source) };
    }

    pub fn deinit(self: *Parser) void {
        self.arena.deinit();
    }

    /// Parses and returns the next entry in the lockfile.
    /// Returns null if end of the file is reached, returns error if there is a syntax error.
    pub fn next(self: *Parser) !?Entry {
        return switch (self.scanner.peek()) {
            .comment => .{ .comment = self.parseComment() },
            .literal, .string => .{ .block = try self.parseBlock() },
            .end_of_file => null,
            else => {
                std.log.err("Unexpected top level token: {any}\n", .{self.scanner.peek()});
                return error.UnexpectedToken;
            },
        };
    }

    fn parseComment(self: *Parser) string {
        const comment = blk: {
            const comment = self.scanner.next().comment;
            bun.assert(comment[0] == '#');
            break :blk comment[1..];
        };

        const start = std.mem.indexOfNone(u8, comment, " ") orelse return comment;
        return comment[start..];
    }

    fn parseBlock(self: *Parser) !Block {
        const arena = self.arena.allocator();

        const block = try arena.create(Block);
        block.* = Block{
            .resolved = "",
            .version = "",
            .packages = std.ArrayList(Block.Package).init(arena),
            .dependencies = std.ArrayList(Block.Package).init(arena),
            .optionalDependencies = std.ArrayList(Block.Package).init(arena),
        };

        while (true) {
            switch (self.scanner.next()) {
                .colon => break,
                .comma, .space => continue,

                .literal => |package| {
                    if (self.scanner.next() != .at) return error.UnexpectedToken;
                    const version = self.scanner.next();
                    if (version != .literal) return error.UnexpectedToken;

                    try block.packages.append(Block.Package{
                        .name = package,
                        .version = version.literal,
                    });
                },

                .string => |package_version_quoted| {
                    if (package_version_quoted.len < 2 or
                        package_version_quoted[0] != '"' or
                        package_version_quoted[package_version_quoted.len - 1] != '"') return error.InvalidString;

                    const package_version = package_version_quoted[1 .. package_version_quoted.len - 1];

                    // Package names can start with '@' for namespaced packages,
                    // after that, first '@' will be the seperator for the version.
                    const version_seperator_pos_search_start: usize = if (package_version[0] == '@') 1 else 0;
                    const version_seperator_pos = std.mem.indexOfScalarPos(u8, package_version, version_seperator_pos_search_start, '@') orelse return error.InvalidPackageVersion;

                    const package = package_version[0..version_seperator_pos];
                    const version = package_version[version_seperator_pos + 1 ..];
                    try block.packages.append(Block.Package{
                        .name = package,
                        .version = version,
                    });
                },

                .end_of_file => return error.UnexpectedEndOfFile,
                else => return error.UnexpectedToken,
            }
        }

        var current_indentation: usize = self.parseSpaces();
        // This is used to track if we parse some spaces,
        // and then realise they weren't part of the current block,
        // so we count as indentation in the next run.
        var additional_indentation: usize = 0;
        while (current_indentation != 0) : ({
            current_indentation = self.parseSpaces() + additional_indentation;
            additional_indentation = 0;
        }) {
            const key = blk: {
                const token = self.scanner.next();
                switch (token) {
                    .literal => |literal| break :blk literal,
                    else => {
                        std.log.err("Unexpected token: {any}\n", .{token});
                        return error.UnexpectedToken;
                    },
                }
            };

            if (std.mem.eql(u8, key, "version")) {
                if (self.parseSpaces() == 0) return error.ExpectedSpace;
                block.version = try self.parseStringOrLiteral();
            } else if (std.mem.eql(u8, key, "resolved")) {
                if (self.parseSpaces() == 0) return error.ExpectedSpace;
                block.resolved = try self.parseStringOrLiteral();
            } else if (std.mem.eql(u8, key, "integrity")) {
                if (self.parseSpaces() == 0) return error.ExpectedSpace;
                block.integrity = try self.parseStringOrLiteral();
            } else if (std.mem.eql(u8, key, "dependencies") or
                std.mem.eql(u8, key, "optionalDependencies"))
            {
                if (self.scanner.next() != .colon) return error.ExpectedColon;

                const deps = if (std.mem.eql(u8, key, "dependencies"))
                    &block.dependencies
                else
                    &block.optionalDependencies;

                var indent = self.parseSpaces();
                while (indent > current_indentation) : (indent = self.parseSpaces()) {
                    const package = try self.parseStringOrLiteral();

                    if (self.parseSpaces() == 0) return error.ExpectedSpace;
                    const version = try self.parseStringOrLiteral();

                    try deps.*.append(Block.Package{
                        .name = package,
                        .version = version,
                    });
                }

                additional_indentation = indent;
            } else {
                if (self.parseSpaces() == 0) return error.ExpectedSpace;
                const val = try self.parseStringOrLiteral();
                std.log.warn("Ignoring unknown key-value: {s} = {s}\n", .{ key, val });
            }
        }

        return block.*;
    }

    fn parseStringOrLiteral(self: *Parser) !string {
        const token = self.scanner.next();
        return switch (token) {
            .literal => |literal| literal,
            .string => |quoted_string| {
                if (quoted_string.len < 2 or
                    quoted_string[0] != '"' or
                    quoted_string[quoted_string.len - 1] != '"') return error.InvalidString;
                return quoted_string[1 .. quoted_string.len - 1];
            },
            else => {
                std.log.err("Expected string or literal but found: {any}\n", .{token});
                return error.UnexpectedToken;
            },
        };
    }

    fn parseSpaces(self: *Parser) usize {
        var size: usize = 0;
        while (self.scanner.peek() == .space) : (size += 1) {
            _ = self.scanner.next();
        }
        return size;
    }
};

/// For JS tests, see `internal-for-testing.ts`.
pub const YarnLockParserTestingAPIs = struct {
    const JSC = bun.JSC;

    pub fn parse(
        globalThis: *JSC.JSGlobalObject,
        callframe: *JSC.CallFrame,
    ) callconv(.C) JSC.JSValue {
        const arguments_ = callframe.arguments(1);
        const arguments = arguments_.slice();

        const jsstr = arguments[0];
        const bunstr = jsstr.toBunString(globalThis);
        defer bunstr.deref();
        const utf8str = bunstr.toUTF8(bun.default_allocator);
        defer utf8str.deinit();

        var parser = Parser.init(bun.default_allocator, utf8str.slice());
        defer parser.deinit();

        var array = JSC.JSValue.createEmptyArray(globalThis, 0);
        var i: u32 = 0;

        while (parser.next() catch |err| {
            std.log.err("Failed to parse yarn lockfile: {}\n", .{err});
            return JSC.JSValue.null;
        }) |entry| {
            array.putIndex(globalThis, i, switch (entry) {
                .comment => zigToJS(globalThis, .{ .comment = entry.comment }),
                .block => zigToJS(globalThis, .{
                    .block = .{
                        .version = entry.block.version,
                        .resolved = entry.block.resolved,
                        .integrity = entry.block.integrity,
                        .dependencies = entry.block.dependencies.items,
                        .optionalDependencies = entry.block.optionalDependencies.items,
                        .packages = entry.block.packages.items,
                    },
                }),
            });
            i += 1;
        }

        return array;
    }

    fn zigToJS(global: *JSC.JSGlobalObject, Val: anytype) JSC.JSValue {
        const T = @TypeOf(Val);

        if (T == JSC.JSValue) return Val;

        switch (@typeInfo(T)) {
            .Pointer => {
                if (bun.trait.isZigString(T)) {
                    return JSC.ZigString.init(Val).toJS(global);
                } else if (bun.trait.isSlice(T)) {
                    var array = JSC.JSValue.createEmptyArray(global, Val.len);
                    for (Val, 0..) |item, i| {
                        array.putIndex(global, @intCast(i), zigToJS(global, item));
                    }
                    return array;
                }

                @compileLog(T);
                @compileError("Unsupported pointer type");
            },
            .Struct => |@"struct"| {
                const fields = @"struct".fields;

                var obj = JSC.JSValue.createEmptyObject(global, fields.len);
                inline for (fields) |field| {
                    const name = field.name;
                    const value = zigToJS(global, @field(Val, name));
                    obj.put(global, JSC.ZigString.static(name), value);
                }
                return obj;
            },
            .Optional => {
                if (Val) |val| {
                    return zigToJS(global, val);
                } else {
                    return JSC.JSValue.null;
                }
            },
            else => {
                @compileLog(T);
                @compileError("Unsupported type");
            },
        }
    }
};
