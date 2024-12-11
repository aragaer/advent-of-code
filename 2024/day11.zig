const std = @import("std");

const info = std.debug.print;

var counter: std.AutoHashMap(u64, usize) = undefined;

inline fn blink_result(value: u64, buf: []u64) ![]u64 {
    buf[0] = 1;
    buf[1] = value * 2024;
    if (value == 0)
        return buf[0..1];
    const len = 1 + std.math.log10_int(value);
    if (@mod(len, 2) == 1)
        return buf[1..2];
    const tens = if (std.math.powi(u64, 10, len / 2)) |x|
        x
    else |_|
        @panic("failed to powi");
    buf[0] = @mod(value, tens);
    buf[1] = value / tens;
    return buf[0..2];
}

fn blink() !usize {
    var copy = try counter.clone();
    defer copy.deinit();
    var iter = copy.iterator();
    var result: usize = 0;
    while (iter.next()) |entry| {
        const value = entry.key_ptr.*;
        const cnt = entry.value_ptr.*;
        var buf = [_]u64{ 0, 0 };
        const new_values = try blink_result(value, &buf);
        if (new_values.len == 2)
            result += cnt;
        for (new_values) |new|
            try counter.put(new, cnt + (counter.get(new) orelse 0));
        const counted = counter.get(value) orelse 0;
        if (counted == cnt)
            _ = counter.remove(value)
        else
            try counter.put(value, counted - cnt);
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    counter = @TypeOf(counter).init(allocator);
    defer counter.deinit();

    var len: usize = 0;
    const reader = std.io.getStdIn().reader();
    if (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |buffer| {
        defer allocator.free(buffer);
        var nums = std.mem.tokenizeAny(u8, buffer, " \t");
        while (nums.next()) |num| {
            const value = try std.fmt.parseInt(u64, num, 10);
            const cnt = counter.get(value) orelse 0;
            try counter.put(value, cnt + 1);
            len += 1;
        }
    }

    inline for (0..25) |_| len += try blink();
    info("{}\n", .{len});
    inline for (0..50) |_| len += try blink();
    info("{}\n", .{len});
}
