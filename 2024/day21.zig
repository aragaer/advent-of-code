const std = @import("std");

const info = std.debug.print;

const numpad: [4][]const u8 = .{ "789", "456", "123", " 0A" };
const dirpad: [2][]const u8 = .{ " ^B", "<v>" };

var coords: std.AutoHashMap(u8, [2]usize) = undefined;

fn fill(pad: []const []const u8) !void {
    for (pad, 0..) |line, y| {
        for (line, 0..) |char, x|
            try coords.put(char, .{ y, x });
    }
}

const Cache = std.AutoHashMap([2]u8, usize);
const Layer = struct {
    cache: Cache,
    prev: ?*Layer,

    const This = @This();

    fn one_move(self: *This, f: u8, s: u8, is_dirpad: bool) usize {
        const a = self.cache.allocator;
        const key: [2]u8 = .{ f, s };
        const res = self.cache.getOrPut(key) catch @panic("mem");
        if (res.found_existing)
            return res.value_ptr.*;

        const p1 = coords.get(f).?;
        const p2 = coords.get(s).?;

        var buf = std.ArrayList(u8).init(a);
        defer buf.deinit();

        move(p1, p2, &buf) catch @panic("mem");

        if (self.prev == null) {
            const result = buf.items.len;
            res.value_ptr.* = result;
            return result;
        }

        const p = self.prev.?;
        const butlast = buf.items[0..(buf.items.len - 1)];
        var best: ?usize = null;
        const result = permute(a, butlast);
        defer a.free(result);
        for (result) |perm| {
            defer a.free(perm);
            if (!check(p1, perm, is_dirpad))
                continue;
            const m = a.alloc(u8, buf.items.len) catch @panic("mem");
            defer a.free(m);
            @memcpy(m[0..butlast.len], perm);
            m[butlast.len] = 'B';
            const attempt = p.compute(m, true);
            if (best == null or attempt < best.?)
                best = attempt;
        }
        res.value_ptr.* = best.?;
        return best.?;
    }

    fn compute(self: *This, code: []const u8, is_dirpad: bool) usize {
        var result: usize = 0;
        var f: u8 = if (is_dirpad) 'B' else 'A';
        for (code) |s| {
            result += self.one_move(f, s, is_dirpad);
            f = s;
        }
        return result;
    }

    fn init(allocator: std.mem.Allocator, prev: ?*Layer) Layer {
        return Layer{ .cache = Cache.init(allocator), .prev = prev };
    }

    fn deinit(self: *This) void {
        self.cache.deinit();
    }
};

fn permute(allocator: std.mem.Allocator, data: []const u8) [][]u8 {
    var perms = std.ArrayList([]u8).init(allocator);
    defer perms.deinit();
    if (data.len == 0) {
        const empty = allocator.alloc(u8, 0) catch @panic("mem");
        perms.append(empty) catch @panic("mem");
    } else {
        for (data, 0..) |item, idx| {
            const without = allocator.alloc(u8, data.len - 1) catch @panic("mem");
            defer allocator.free(without);
            @memcpy(without[0..idx], data[0..idx]);
            @memcpy(without[idx..], data[(idx + 1)..]);
            const result = permute(allocator, without);
            defer allocator.free(result);
            for (result) |piece| {
                defer allocator.free(piece);
                const new = allocator.alloc(u8, data.len) catch @panic("mem");
                new[0] = item;
                @memcpy(new[1..], piece);
                perms.append(new) catch @panic("mem");
            }
        }
    }
    return (allocator.dupe([]u8, perms.items) catch @panic("mem"));
}

fn move(f: [2]usize, t: [2]usize, buf: *std.ArrayList(u8)) !void {
    var x = f[1];
    var y = f[0];
    while (x < t[1]) : (x += 1)
        try buf.append('>');
    while (y > t[0]) : (y -= 1)
        try buf.append('^');
    while (x > t[1]) : (x -= 1)
        try buf.append('<');
    while (y < t[0]) : (y += 1)
        try buf.append('v');
    try buf.append('B');
}

fn check(p: [2]usize, moves: []const u8, is_dirpad: bool) bool {
    var x = p[1];
    var y = p[0];
    for (moves) |m| {
        switch (m) {
            '<' => x -= 1,
            '>' => x += 1,
            '^' => y -= 1,
            'v' => y += 1,
            else => {},
        }
        if ((if (is_dirpad)
            dirpad[y][x]
        else
            numpad[y][x]) == ' ')
            return false;
    }
    return true;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    coords = @TypeOf(coords).init(allocator);
    defer coords.deinit();

    var layers = try allocator.alloc(Layer, 26);
    defer {
        for (layers) |*layer|
            layer.deinit();
        allocator.free(layers);
    }

    try fill(&numpad);
    try fill(&dirpad);

    for (0..layers.len) |idx| {
        const prev = if (idx == 0) null else &layers[idx - 1];
        layers[idx] = Layer.init(allocator, prev);
    }

    var part1: usize = 0;
    var part2: usize = 0;
    var buf: [200]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const n = try std.fmt.parseInt(usize, line[0..(line.len - 1)], 10);
        part1 += n * layers[2].compute(line, false);
        part2 += n * layers[25].compute(line, false);
    }

    info("{}\n{}\n", .{ part1, part2 });
}
