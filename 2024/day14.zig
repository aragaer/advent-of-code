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
const Robot = struct {
    p: Coord,
    v: Coord,
    fn move(s: *Robot) !void {
        try robots_on_map.put(s.*.p, robots_on_map.get(s.*.p).? - 1);
        const nx = @mod(s.*.p.re + s.*.v.re, @as(T, @intCast(width)));
        const ny = @mod(s.*.p.im + s.*.v.im, @as(T, @intCast(height)));
        s.*.p = c(nx, ny);
        try robots_on_map.put(s.*.p, 1 + (robots_on_map.get(s.*.p) orelse 0));
    }
};

var robots: std.ArrayList(Robot) = undefined;
var robots_on_map: std.AutoHashMap(Coord, usize) = undefined;
var seen: std.AutoHashMap(Coord, void) = undefined;
var queue: std.ArrayList(Coord) = undefined;
var width: usize = 101;
var height: usize = 103;

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(i32, @intCast(x)), @as(i32, @intCast(y)));
}

fn is_tree() !bool {
    seen.clearRetainingCapacity();
    queue.clearRetainingCapacity();
    var count1: usize = 0;
    var count2: usize = 0;
    for (robots.items) |robot| {
        if (seen.contains(robot.p))
            continue;
        var cluster_size: usize = 1;
        queue.clearRetainingCapacity();
        try queue.append(robot.p);
        while (queue.popOrNull()) |pos|
            for (directions) |dir| {
                const npos = pos.add(dir);
                if (seen.contains(npos))
                    continue;
                if (robots_on_map.get(npos) orelse 0 > 0) {
                    try seen.put(npos, void{});
                    try queue.append(npos);
                    cluster_size += 1;
                }
            };
        if (cluster_size > count1) {
            count2 = count1;
            count1 = cluster_size;
        } else count2 = @max(count2, cluster_size);
    }
    if (count1 + count2 > @divTrunc(robots.items.len, 2))
        return true;

    return false;
}

fn part1() void {
    var counts = [4]usize{ 0, 0, 0, 0 };
    const hw = @divTrunc(width, 2);
    const hh = @divTrunc(height, 2);
    for (robots.items) |robot| {
        if (robot.p.re == hw or robot.p.im == hh)
            continue;
        const h = @as(usize, @intFromBool(robot.p.re > hw));
        const v = @as(usize, @intFromBool(robot.p.im > hh));
        counts[h + v * 2] += 1;
    }
    var result: usize = 1;
    for (counts) |cnt|
        result *= cnt;
    info("{}\n", .{result});
}

fn dump() void {
    var pos: usize = 0;
    for (0..height) |y| {
        for (0..width) |x| {
            const here = robots_on_map.get(c(x, y)) orelse 0;
            if (here > 0)
                info("#", .{})
            else
                info(".", .{});
            pos += 0;
        }
        info("\n", .{});
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    robots = @TypeOf(robots).init(allocator);
    defer robots.deinit();
    robots_on_map = @TypeOf(robots_on_map).init(allocator);
    defer robots_on_map.deinit();
    seen = @TypeOf(seen).init(allocator);
    defer seen.deinit();
    queue = @TypeOf(queue).init(allocator);
    defer queue.deinit();

    var buf: [100]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var pv = std.mem.tokenizeScalar(u8, line, ' ');
        var d: [4]T = undefined;
        inline for (0..2) |a| {
            var w = std.mem.tokenizeScalar(u8, pv.next().?[2..], ',');
            inline for (0..2) |b|
                d[a * 2 + b] = try std.fmt.parseInt(T, w.next().?, 10);
        }
        const p = c(d[0], d[1]);
        try robots.append(Robot{ .p = p, .v = c(d[2], d[3]) });
        try robots_on_map.put(p, 1 + (robots_on_map.get(p) orelse 0));
    }

    const is_test = robots.items.len == 12;
    if (is_test) {
        width = 11;
        height = 7;
    }

    info("{} {}\n", .{ width * height, @mod(100, width * height) });

    var seconds: usize = 0;
    while ((is_test or !try is_tree()) and seconds < width * height) : (seconds += 1) {
        if (seconds == @mod(100, width * height)) {
            part1();
            if (is_test)
                return;
        }
        for (robots.items) |*robot|
            try robot.*.move();
    }
    info("{}\n", .{seconds});
}
