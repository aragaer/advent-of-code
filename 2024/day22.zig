const std = @import("std");

const info = std.debug.print;
const T = usize;
const Key = T;

inline fn mix(num: T, secret: T) T {
    return num ^ secret;
}

inline fn prune(num: T) T {
    return @mod(num, 16777216);
}

inline fn next(num: T) T {
    const s1 = prune(mix(num, num << 6));
    const s2 = prune(mix(s1, s1 >> 5));
    return prune(mix(s2 * 2048, s2));
}

inline fn update_key(key: Key, new: T) Key {
    return (key & 0x7FFF) << 5 | new;
}
var total: [1 << 20]T = undefined;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var one = std.AutoHashMap(Key, T).init(allocator);
    defer one.deinit();

    var part1: T = 0;
    var buf: [200]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        one.clearRetainingCapacity();
        var key: Key = 0;
        var n = try std.fmt.parseInt(T, line, 10);
        var digit = @mod(n, 10);
        inline for (0..3) |_| {
            n = next(n);
            const nd = @mod(n, 10);
            key = update_key(key, 10 + nd - digit);
            digit = nd;
        }
        for (3..2000) |_| {
            n = next(n);
            const nd = @mod(n, 10);
            key = update_key(key, 10 + nd - digit);
            const res = try one.getOrPut(key);
            if (!res.found_existing)
                res.value_ptr.* = nd;
            digit = nd;
        }
        part1 += n;
        var iter = one.iterator();
        while (iter.next()) |entry|
            total[entry.key_ptr.*] += entry.value_ptr.*;
    }
    info("{}\n", .{part1});

    var res: T = 0;
    for (total) |item|
        res = @max(res, item);
    info("{}\n", .{res});
}
