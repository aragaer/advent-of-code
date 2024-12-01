const std = @import("std");

const info = std.debug.print;
const Allocator = std.mem.Allocator;
const List = std.ArrayList(i32);
const Parser = std.fmt.parseInt;

fn get_input(allocator: Allocator) !struct { first: List, second: List } {
    const reader = std.io.getStdIn().reader();
    var buffer: [100]u8 = undefined;
    var result = .{ .first = List.init(allocator), .second = List.init(allocator) };
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var words = std.mem.tokenizeAny(u8, line, " \t");
        try result.first.append(try Parser(i32, words.next().?, 10));
        try result.second.append(try Parser(i32, words.next().?, 10));
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const data = try get_input(allocator);
    defer data.first.deinit();
    defer data.second.deinit();

    std.mem.sort(i32, data.first.items, {}, comptime std.sort.asc(i32));
    std.mem.sort(i32, data.second.items, {}, comptime std.sort.asc(i32));

    var firstMap = std.AutoHashMap(i32, i32).init(allocator);
    var secondMap = std.AutoHashMap(i32, i32).init(allocator);
    defer firstMap.deinit();
    defer secondMap.deinit();

    var part1: u32 = 0;
    for (data.first.items, data.second.items) |f, s| {
        part1 += @abs(f - s);
        const fv = try firstMap.getOrPut(f);
        const sv = try secondMap.getOrPut(s);
        fv.value_ptr.* = 1 + if (fv.found_existing) fv.value_ptr.* else 0;
        sv.value_ptr.* = 1 + if (sv.found_existing) sv.value_ptr.* else 0;
    }
    info("{}\n", .{part1});

    var part2: i32 = 0;
    var iter = firstMap.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*;
        part2 += key * entry.value_ptr.* * (secondMap.get(key) orelse 0);
    }
    info("{}\n", .{part2});
}
