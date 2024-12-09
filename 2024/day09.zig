const std = @import("std");

const info = std.debug.print;
const stat = struct { u8, usize };

inline fn checksum(ptr: *usize, cnt: usize, mul: usize) usize {
    var result: usize = 0;
    for (0..cnt) |i|
        result += (ptr.* + i) * mul;
    ptr.* += cnt;
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var files = std.ArrayList(stat).init(allocator);
    defer files.deinit();
    var spaces = std.ArrayList(stat).init(allocator);
    defer spaces.deinit();

    const reader = std.io.getStdIn().reader();

    var total_size: usize = 0;
    var ptr: usize = 0;
    var is_file = true;
    while (reader.readByte()) |char| : (is_file = !is_file) {
        const len = std.fmt.charToDigit(char, 10) catch break;
        try (if (is_file) files else spaces).append(.{ len, ptr });
        if (is_file)
            total_size += len;
        ptr += len;
    } else |_| {}
    if (!is_file)
        try spaces.append(.{ 0, ptr });

    var part1: usize = 0;
    var tail_file: usize = files.items.len - 1;
    var taken: usize = 0;
    ptr = 0;
    for (files.items, spaces.items, 0..) |file, space, idx| {
        part1 += checksum(&ptr, @min(file[0], total_size - ptr), idx);
        var space_fill = @min(space[0], total_size - ptr);
        while (space_fill > 0 and tail_file >= idx) {
            const take = @min(files.items[tail_file][0] - taken, space_fill);
            part1 += checksum(&ptr, take, tail_file);
            taken += take;
            space_fill -= take;
            if (taken == files.items[tail_file][0]) {
                tail_file -= 1;
                taken = 0;
            }
        }
    }

    var part2: usize = 0;
    var fileno = files.items.len - 1;
    while (fileno > 0) : (fileno -= 1) {
        const file = files.items[fileno];
        var position = file[1];
        for (0..fileno) |space|
            if (spaces.items[space][0] >= file[0]) {
                position = spaces.items[space][1];
                spaces.items[space][0] -= file[0];
                spaces.items[space][1] += file[0];
                break;
            };
        part2 += checksum(&position, file[0], fileno);
    }
    info("{}\n{}\n", .{ part1, part2 });
}
