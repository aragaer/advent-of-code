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
    return char == '[' or char == ']' or char == 'O';
}

var map: std.AutoHashMap(Coord, u8) = undefined;

fn move(command: u8, allocator: std.mem.Allocator) !void {
    const direction = switch (command) {
        '>' => c(1, 0),
        '^' => c(0, -1),
        'v' => c(0, 1),
        '<' => c(-1, 0),
        else => @panic("unknown movement"),
    };
    if (direction.im == 0) {
        var pos = robot.add(direction);
        while (is_box(map.get(pos))) : (pos = pos.add(direction)) {}
        if (map.get(pos) == '#')
            return;
        while (pos.re != robot.re) : (pos = pos.sub(direction))
            try map.put(pos, map.get(pos.sub(direction)) orelse '.');
    } else {
        var layer = std.AutoHashMap(T, void).init(allocator);
        defer layer.deinit();
        try layer.put(robot.re, void{});
        const ydir = direction.im;
        var y = robot.im + ydir;
        var boxes = std.AutoHashMap(Coord, bool).init(allocator);
        defer boxes.deinit();
        while (layer.count() > 0) : (y += ydir) {
            var next_layer = @TypeOf(layer).init(allocator);
            var iter = layer.keyIterator();
            while (iter.next()) |p| {
                const pos = p.*;
                switch (map.get(c(pos, y)) orelse '.') {
                    '[' => {
                        try next_layer.put(pos, void{});
                        try next_layer.put(pos + 1, void{});
                        try boxes.put(c(pos, y), true);
                    },
                    ']' => {
                        try next_layer.put(pos - 1, void{});
                        try next_layer.put(pos, void{});
                        try boxes.put(c(pos - 1, y), true);
                    },
                    '#' => {
                        next_layer.deinit();
                        return;
                    },
                    'O' => {
                        try next_layer.put(pos, void{});
                        try boxes.put(c(pos, y), false);
                    },
                    else => {},
                }
            }
            layer.deinit();
            layer = next_layer;
        }
        if (boxes.count() > 0) {
            var iter = boxes.iterator();
            while (iter.next()) |entry| {
                try map.put(entry.key_ptr.*, '.');
                if (entry.value_ptr.*)
                    try map.put(entry.key_ptr.*.add(c(1, 0)), '.');
            }
            iter = boxes.iterator();
            while (iter.next()) |entry| {
                const pos = entry.key_ptr.*.add(direction);
                if (entry.value_ptr.*) {
                    try map.put(pos, '[');
                    try map.put(pos.add(c(1, 0)), ']');
                } else try map.put(pos, 'O');
            }
        }
    }
    robot = robot.add(direction);
}

fn dump() void {
    for (0..height) |y| {
        for (0..width) |x| {
            if (x == robot.re and y == robot.im)
                info("@", .{})
            else
                info("{c}", .{map.get(c(x, y)) orelse '.'});
        }
        info("\n", .{});
    }
}

fn gps_sum() T {
    var result: T = 0;
    var iter = map.iterator();
    while (iter.next()) |entry| {
        if (entry.value_ptr.* == '[' or entry.value_ptr.* == 'O')
            result += entry.key_ptr.*.re + entry.key_ptr.*.im * 100;
    }
    return result;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    map = @TypeOf(map).init(allocator);
    var map2 = @TypeOf(map).init(allocator);
    var robot2: Coord = undefined;

    var buf: [4096]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    height = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| : (height += 1) {
        if (line.len == 0)
            break;
        width = line.len * 2;
        for (line, 0..) |char, idx| {
            switch (char) {
                '@' => {
                    robot = c(idx, height);
                    robot2 = c(idx * 2, height);
                },
                'O' => {
                    try map.put(c(idx, height), 'O');
                    try map2.put(c(idx * 2, height), '[');
                    try map2.put(c(idx * 2 + 1, height), ']');
                },
                '#' => {
                    try map.put(c(idx, height), '#');
                    try map2.put(c(idx * 2, height), '#');
                    try map2.put(c(idx * 2 + 1, height), '#');
                },
                else => {},
            }
        }
    }
    var commands = std.ArrayList(u8).init(allocator);
    defer commands.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line|
        try commands.appendSlice(line);

    for (commands.items) |command|
        try move(command, allocator);

    info("{}\n", .{gps_sum()});
    map.deinit();
    map = map2.move();
    robot = robot2;
    for (commands.items) |command|
        try move(command, allocator);

    info("{}\n", .{gps_sum()});
    map.deinit();
}
