const std = @import("std");

const info = std.debug.print;
const Coord = std.math.Complex(i32);

const directions = [_]Coord{
    Coord.init(1, 0),
    Coord.init(-1, 0),
    Coord.init(0, 1),
    Coord.init(0, -1),
    Coord.init(1, 1),
    Coord.init(1, -1),
    Coord.init(-1, 1),
    Coord.init(-1, -1),
};

var data: std.ArrayList([]u8) = undefined;

inline fn get(pos: Coord) ?u8 {
    if (pos.re < 0 or pos.re >= data.items.len)
        return null;
    if (pos.im < 0 or pos.im >= data.items.len)
        return null;
    return data.items[@as(usize, @intCast(pos.im))][@as(usize, @intCast(pos.re))];
}

inline fn c(x: anytype, y: anytype) Coord {
    return Coord.init(@as(i32, @intCast(x)), @as(i32, @intCast(y)));
}

fn check_word(word: []const u8, start: Coord, dir: Coord) bool {
    var here = start;
    for (word) |char| {
        if (get(here) != char)
            return false;
        here = here.add(dir);
    }
    return true;
}

fn count_xmas(pos: Coord) u32 {
    var count: u32 = 0;
    for (directions) |direction| {
        if (check_word("XMAS", pos, direction))
            count += 1;
    }
    return count;
}

fn check_x_mas(pos: Coord) bool {
    const checks = [_]Coord{ c(1, 1), c(1, -1) };
    for (checks) |check| {
        if (check_word("MAS", pos.add(check), check.neg()))
            continue;
        if (check_word("MAS", pos.sub(check), check))
            continue;
        return false;
    }
    return true;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    data = @TypeOf(data).init(allocator);
    defer {
        for (data.items) |line|
            allocator.free(line);
        data.deinit();
    }

    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEofAlloc(allocator, '\n', 1024)) |buffer|
        try data.append(buffer);
    info("Data: {} by {}\n", .{ data.items.len, data.items[0].len });

    var part1: u32 = 0;
    var part2: u32 = 0;
    for (data.items, 0..) |row, y|
        for (row, 0..) |_, x| {
            part1 += count_xmas(c(x, y));
            if (check_x_mas(c(x, y)))
                part2 += 1;
        };
    info("{}\n{}\n", .{ part1, part2 });
}
