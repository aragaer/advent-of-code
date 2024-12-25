const std = @import("std");

const info = std.debug.print;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var keys = std.ArrayList([5]u8).init(allocator);
    defer keys.deinit();
    var locks = std.ArrayList([5]u8).init(allocator);
    defer locks.deinit();

    var current: ?*[5]u8 = null;
    var buf: [200]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line|
        if (line.len == 0) {
            current = null;
        } else if (current == null) {
            current = try (if (line[0] == '#') locks else keys).addOne();
            inline for (0..5) |i|
                current.?.*[i] = 0;
        } else inline for (0..5) |i| {
            current.?.*[i] += @as(u8, @intCast(@intFromBool(line[i] == '#')));
        };

    var res: usize = 0;
    for (locks.items) |lock| for (keys.items) |key| {
        inline for (0..5) |i|
            res += @as(usize, @intCast(@intFromBool(lock[i] + key[i] < 7)));
        res -= @mod(res, 5);
    };
    info("{}\n", .{@divTrunc(res, 5)});
}
