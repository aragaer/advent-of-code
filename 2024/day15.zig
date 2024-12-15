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

var map: std.AutoHashMap(Coord, u8) = undefined;
var robot: Coord = undefined;
var width: usize = undefined;
var height: usize = undefined;

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(T, @intCast(x)), @as(T, @intCast(y)));
}

fn move(command: u8) !void {
    const direction = switch (command) {
        '>' => c(1, 0),
        '^' => c(0, -1),
        'v' => c(0, 1),
        '<' => c(-1, 0),
        else => @panic("unknown movement"),
    };
    var pos = robot.add(direction);
    while (map.get(pos) == 'O') : (pos = pos.add(direction)) {}
    if (map.get(pos) != '#') { // empty space
        robot = robot.add(direction);
        if (map.get(robot) == 'O') {
            try map.put(pos, 'O');
            try map.put(robot, '.');
        }
    }
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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    map = @TypeOf(map).init(allocator);
    defer map.deinit();

    var buf: [4096]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    height = 0;
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| : (height += 1) {
        if (line.len == 0)
            break;
        width = line.len;
        for (line, 0..) |char, idx| {
            switch (char) {
                '@' => robot = c(idx, height),
                'O', '#' => try map.put(c(idx, height), char),
                else => {},
            }
        }
    }
    var commands = std.ArrayList(u8).init(allocator);
    defer commands.deinit();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line|
        try commands.appendSlice(line);

    for (commands.items) |command|
        try move(command);

    var part1: T = 0;
    var iter = map.iterator();
    while (iter.next()) |entry| {
        if (entry.value_ptr.* == 'O')
            part1 += entry.key_ptr.*.re + entry.key_ptr.*.im * 100;
    }

    info("{}\n", .{part1});
}
