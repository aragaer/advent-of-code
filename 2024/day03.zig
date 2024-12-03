const std = @import("std");

const info = std.debug.print;

fn read_int(data: []u8) !struct { value: u64, len: usize } {
    var value: u64 = 0;
    const len = loop: for (data, 0..) |char, idx| {
        if (std.fmt.charToDigit(char, 10)) |digit|
            value = value * 10 + digit
        else |_|
            break :loop idx;
    } else 0;
    if (len > 0)
        return .{ .value = value, .len = len }
    else
        return error.NoDigit;
}

// inline fn starts_with(data: []u8, start: []u8) {
//     const len = comptime start.len;
//     return std.mem.eql(u8, data[0..len], start);
// }

fn read_mul(data: []u8) struct { value: ?u64, len: usize } {
    var value: ?u64 = null;
    var idx: usize = 1;
    try_read: {
        if (!std.mem.eql(u8, data[0..4], "mul("))
            break :try_read;
        idx = 4;
        const result1 = read_int(data[idx..]) catch break :try_read;
        idx += result1.len;
        if (data[idx] != ',')
            break :try_read;
        idx += 1;
        const result2 = read_int(data[idx..]) catch break :try_read;
        idx += result2.len;
        if (data[idx] != ')')
            break :try_read;
        idx += 1;
        value = result1.value * result2.value;
    }
    return .{ .value = value, .len = idx };
}

fn read_do_dont(data: []u8) struct { value: ?bool, len: usize } {
    if (std.mem.eql(u8, data[0..4], "do()"))
        return .{ .value = true, .len = 4 };
    if (std.mem.eql(u8, data[0..7], "don't()"))
        return .{ .value = false, .len = 7 };
    return .{ .value = null, .len = 1 };
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var data = std.ArrayList(u8).init(allocator);
    defer data.deinit();

    const reader = std.io.getStdIn().reader();
    try reader.readAllArrayList(&data, comptime (128 * 1024));

    var part1: u64 = 0;
    var part2: u64 = 0;
    var enabled = true;
    var idx: usize = 0;
    while (std.mem.indexOfAnyPos(u8, data.items, idx, "md")) |pos|
        switch (data.items[pos]) {
            'm' => {
                const result = read_mul(data.items[pos..]);
                idx = pos + result.len;
                if (result.value) |value| {
                    part1 += value;
                    if (enabled)
                        part2 += value;
                }
            },
            'd' => {
                const result = read_do_dont(data.items[pos..]);
                idx = pos + result.len;
                if (result.value) |value|
                    enabled = value;
            },
            else => @panic("should be either m or d only"),
        };
    info("{}\n{}\n", .{ part1, part2 });
}
