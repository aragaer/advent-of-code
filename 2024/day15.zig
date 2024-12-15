const std = @import("std");

const info = std.debug.print;
const T = i32;
const Coord = std.math.Complex(T);

var robot: Coord = undefined;
var width: usize = undefined;
var height: usize = undefined;

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(T, @intCast(x)), @as(T, @intCast(y)));
}

inline fn is_box(char: ?u8) bool {
    return char == '[' or char == ']';
}

var map: std.AutoHashMap(Coord, u8) = undefined;
var commands: std.ArrayList(u8) = undefined;

fn move(command: u8, re_mul: T, allocator: std.mem.Allocator) !void {
    const dir = switch (command) {
        '>' => c(re_mul, 0),
        '^' => c(0, -1),
        'v' => c(0, 1),
        '<' => c(-re_mul, 0),
        else => @panic("unknown movement"),
    };
    var boxes = std.AutoHashMap(Coord, void).init(allocator);
    defer boxes.deinit();
    if (dir.im == 0) {
        var pos = robot.add(dir);
        while (is_box(map.get(pos))) : (pos = pos.add(dir)) {
            if (map.get(pos) == '[')
                try boxes.put(pos, void{});
        }

        if (map.get(pos) == '#')
            return;
    } else {
        var layer = std.AutoHashMap(T, void).init(allocator);
        defer layer.deinit();
        try layer.put(robot.re, void{});
        var y = robot.im + dir.im;
        while (layer.count() > 0) : (y += dir.im) {
            var next_layer = @TypeOf(layer).init(allocator);
            var iter = layer.keyIterator();
            while (iter.next()) |p| {
                const char = map.get(c(p.*, y)) orelse '.';
                switch (char) {
                    '[', ']' => {
                        const o: T = if (char == '[') 0 else 1;
                        try next_layer.put(p.* - o, void{});
                        try next_layer.put(p.* + 1 - o, void{});
                        try boxes.put(c(p.* - o, y), void{});
                    },
                    '#' => {
                        next_layer.deinit();
                        return;
                    },
                    else => {},
                }
            }
            layer.deinit();
            layer = next_layer;
        }
    }
    var iter = boxes.keyIterator();
    while (iter.next()) |p| {
        _ = map.remove(p.*);
        _ = map.remove(p.*.add(c(1, 0)));
    }
    iter = boxes.keyIterator();
    while (iter.next()) |p| {
        const pos = p.*.add(dir);
        try map.put(pos, '[');
        try map.put(pos.add(c(1, 0)), ']');
    }

    robot = robot.add(dir);
}

fn dump() void {
    for (0..height) |y| {
        for (0..width) |x|
            if (x == robot.re and y == robot.im)
                info("@", .{})
            else
                info("{c}", .{map.get(c(x, y)) orelse '.'});
        info("\n", .{});
    }
}

fn solve(allocator: std.mem.Allocator, m: @TypeOf(map), r: Coord, part: T) T {
    map = m.clone() catch @panic("no memory");
    defer map.deinit();
    robot = r;
    for (commands.items) |command|
        move(command, 3 - part, allocator) catch @panic("failed to move");
    var result: T = 0;
    var iter = map.iterator();
    while (iter.next()) |entry|
        if (entry.value_ptr.* == '[') {
            const p = entry.key_ptr.*;
            result += @divTrunc(p.re, 3 - part) + p.im * 100;
        };
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var m = @TypeOf(map).init(allocator);
    defer m.deinit();
    var r: Coord = undefined;

    var buf: [4096]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    height = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| : (height += 1) {
        if (line.len == 0)
            break;
        width = line.len * 2;
        for (line, 0..) |char, idx| {
            switch (char) {
                '@' => r = c(idx * 2, height),
                'O' => {
                    try m.put(c(idx * 2, height), '[');
                    try m.put(c(idx * 2 + 1, height), ']');
                },
                '#' => {
                    try m.put(c(idx * 2, height), '#');
                    try m.put(c(idx * 2 + 1, height), '#');
                },
                else => {},
            }
        }
    }
    commands = @TypeOf(commands).init(allocator);
    defer commands.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line|
        try commands.appendSlice(line);
    info("{}\n{}\n", .{ solve(allocator, m, r, 1), solve(allocator, m, r, 2) });
}
