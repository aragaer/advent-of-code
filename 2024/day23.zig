const std = @import("std");

const info = std.debug.print;
const Id = usize;

const Node = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    neighbours: std.AutoHashMap(usize, void),

    const This = @This();

    fn init(allocator: std.mem.Allocator, name: []const u8) This {
        const my_name = name;
        const neighs = std.AutoHashMap(usize, void).init(allocator);
        return Node{ .neighbours = neighs, .name = my_name, .allocator = allocator };
    }

    fn deinit(self: *This) void {
        self.neighbours.deinit();
        self.allocator.free(self.name);
    }

    fn append(self: *This, other: Id) !void {
        try self.neighbours.put(other, void{});
    }
};

var nodes: std.ArrayList(Node) = undefined;

fn nameLessThan(_: void, f: []const u8, s: []const u8) bool {
    return std.mem.lessThan(u8, f, s);
}

fn solve_part1(links: std.ArrayList([2]Id)) !void {
    var triples = std.StringHashMap(void).init(nodes.allocator);
    defer {
        var keys = triples.keyIterator();
        while (keys.next()) |key|
            nodes.allocator.free(key.*);
        triples.deinit();
    }

    for (links.items) |link| {
        const f = link[0];
        const s = link[1];
        const n2 = nodes.items[s].neighbours;
        var iter = nodes.items[f].neighbours.keyIterator();
        while (iter.next()) |nf| {
            if (!n2.contains(nf.*))
                continue;
            var triple: [3][]const u8 = .{ nodes.items[f].name, nodes.items[s].name, nodes.items[nf.*].name };
            std.mem.sort([]const u8, &triple, {}, nameLessThan);
            const sorted = try std.mem.join(nodes.allocator, ",", &triple);
            const r = try triples.getOrPut(sorted);
            if (r.found_existing)
                nodes.allocator.free(sorted);
        }
    }

    var part1: usize = 0;
    var iter = triples.keyIterator();
    while (iter.next()) |triple| {
        if (triple.*[0] == 't' or triple.*[3] == 't' or triple.*[6] == 't')
            part1 += 1;
    }
    info("{}\n", .{part1});
}

// https://www.geeksforgeeks.org/maximal-clique-problem-recursive-solution/

fn solve_part2() !void {}

fn read_data(allocator: std.mem.Allocator) !std.ArrayList([2]Id) {
    var links = std.ArrayList([2]Id).init(allocator);
    var seen = std.StringHashMap(usize).init(allocator);
    defer seen.deinit();

    var buf: [10]u8 = undefined;
    const reader = std.io.getStdIn().reader();
    while (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const n1 = try allocator.dupe(u8, line[0..2]);
        const n2 = try allocator.dupe(u8, line[3..5]);
        const f = try seen.getOrPut(n1);
        if (f.found_existing) {
            allocator.free(n1);
        } else {
            f.value_ptr.* = nodes.items.len;
            try nodes.append(Node.init(allocator, n1));
        }
        const fid = f.value_ptr.*;
        const s = try seen.getOrPut(n2);
        if (s.found_existing) {
            allocator.free(n2);
        } else {
            s.value_ptr.* = nodes.items.len;
            try nodes.append(Node.init(allocator, n2));
        }
        const sid = s.value_ptr.*;
        try links.append([2]Id{ fid, sid });
        try nodes.items[fid].append(sid);
        try nodes.items[sid].append(fid);
    }

    return links;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    nodes = @TypeOf(nodes).init(allocator);
    defer {
        for (nodes.items) |*node|
            node.deinit();
        nodes.deinit();
    }

    const links = try read_data(allocator);
    defer links.deinit();

    try solve_part1(links);
}
