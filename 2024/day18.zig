const std = @import("std");

const info = std.debug.print;
const T = i32;
const Coord = std.math.Complex(T);
const directions = [_]Coord{
    Coord.init(1, 0),
    Coord.init(-1, 0),
    Coord.init(0, 1),
    Coord.init(0, -1),
};

var data: std.ArrayList(Coord) = undefined;

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(T, @intCast(x)), @as(T, @intCast(y)));
}

var width: usize = 70;
var height: usize = 70;
inline fn in_bounds(x: anytype, y: anytype) bool {
    return !(x < 0 or x > width or y < 0 or y > height);
}

fn solve(allocator: std.mem.Allocator, fallen: usize) !?usize {
    var area = std.AutoHashMap(Coord, ?usize).init(allocator);
    defer area.deinit();

    for (0..fallen) |idx|
        try area.put(data.items[idx], null);

    var queue = std.ArrayList(struct { Coord, usize }).init(allocator);
    defer queue.deinit();
    try queue.append(.{ c(0, 0), 0 });
    try area.put(c(0, 0), 0);

    while (queue.popOrNull()) |pt| {
        const cost = pt[1];
        for (directions) |dir| {
            const np = pt[0].add(dir);
            if (!in_bounds(np.re, np.im))
                continue;
            if (np.re == width and np.im == height)
                return cost + 1;
            if (area.contains(np))
                continue;
            try area.put(np, cost + 1);
            try queue.insert(0, .{ np, cost + 1 });
        }
    }
    return null;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    data = @TypeOf(data).init(allocator);
    defer data.deinit();

    var buf: [15]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var pv = std.mem.tokenizeScalar(u8, line, ',');
        const x = try std.fmt.parseInt(T, pv.next().?, 10);
        const y = try std.fmt.parseInt(T, pv.next().?, 10);
        try data.append(c(x, y));
    }

    var fallen: usize = 1024;
    if (data.items.len == 25) {
        width = 6;
        height = 6;
        fallen = 12;
    }

    info("{}\n", .{try solve(allocator, fallen) orelse @panic("fail")});
    var good = fallen;
    var bad = data.items.len;
    while (bad - good > 1) {
        const mid = @divTrunc(good + bad, 2);
        if (try solve(allocator, mid)) |_|
            good = mid
        else
            bad = mid;
    }
    info("{},{}\n", .{ data.items[good].re, data.items[good].im });
}
