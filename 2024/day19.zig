const std = @import("std");

const info = std.debug.print;

var data: std.ArrayList([]const u8) = undefined;

inline fn solve_inner(pattern: []u8) usize {
    if (pattern.len == 0)
        return 1;
    var result: usize = 0;
    for (data.items) |towel| {
        if (std.mem.startsWith(u8, pattern, towel))
            result += solve(pattern[towel.len..]);
    }
    return result;
}

var cache: std.StringHashMap(usize) = undefined;
fn solve(pattern: []u8) usize {
    if (cache.contains(pattern))
        return cache.get(pattern).?;
    const result = solve_inner(pattern);
    const key = cache.allocator.dupe(u8, pattern) catch @panic("can't dupe");
    cache.put(key, result) catch @panic("can't store result");
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    data = @TypeOf(data).init(allocator);
    defer data.deinit();
    cache = @TypeOf(cache).init(allocator);
    defer {
        var keys = cache.keyIterator();
        while (keys.next()) |key|
            allocator.free(key.*);
        cache.deinit();
    }

    var towel_buf: [4096]u8 = undefined;
    const reader = std.io.getStdIn().reader();

    const towels = (try reader.readUntilDelimiterOrEof(&towel_buf, '\n')).?;
    var ti = std.mem.tokenizeSequence(u8, towels, ", ");
    while (ti.next()) |t|
        try data.append(t);
    var buf: [200]u8 = undefined;
    _ = try reader.readUntilDelimiterOrEof(&buf, '\n');

    var part1: usize = 0;
    var part2: usize = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |pattern| {
        const result = solve(pattern);
        if (result > 0)
            part1 += 1;
        part2 += result;
    }

    info("{}\n{}\n", .{ part1, part2 });
}
