const std = @import("std");

const info = std.debug.print;
const Id = usize;
const Name = []const u8;

const Node = struct {
    name: Name,
    neighbours: std.AutoHashMap(usize, void),

    const This = @This();

    fn init(allocator: std.mem.Allocator, name: Name) This {
        const my_name = name;
        const neighs = std.AutoHashMap(usize, void).init(allocator);
        return Node{ .neighbours = neighs, .name = my_name };
    }

    fn deinit(self: *This) void {
        self.neighbours.allocator.free(self.name);
        self.neighbours.deinit();
    }

    inline fn append(self: *This, other: Id) !void {
        try self.neighbours.put(other, void{});
    }
};

var nodes: std.ArrayList(Node) = undefined;
var links: std.AutoHashMap([2]Id, void) = undefined;

fn nameLessThan(_: void, f: Name, s: Name) bool {
    return std.mem.lessThan(u8, f, s);
}

fn solve_part1() !void {
    var triples = std.StringHashMap(void).init(nodes.allocator);
    defer {
        var keys = triples.keyIterator();
        while (keys.next()) |key|
            nodes.allocator.free(key.*);
        triples.deinit();
    }

    var linkiter = links.keyIterator();
    while (linkiter.next()) |link| {
        const f = link[0];
        const s = link[1];
        if (f > s)
            continue;
        const n2 = nodes.items[s].neighbours;
        var iter = nodes.items[f].neighbours.keyIterator();
        while (iter.next()) |nf| {
            if (!n2.contains(nf.*))
                continue;
            var triple: [3]Name = .{ nodes.items[f].name, nodes.items[s].name, nodes.items[nf.*].name };
            std.mem.sort(Name, &triple, {}, nameLessThan);
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

const NodeSet = std.ArrayList(Id);

// https://www.geeksforgeeks.org/maximal-clique-problem-recursive-solution/
fn bronKerbosch(clique: *NodeSet, candidates: *NodeSet, checked: *NodeSet, cliques: *std.ArrayList(NodeSet)) !void {
    defer candidates.deinit();
    defer checked.deinit();
    if (candidates.items.len == 0 and checked.items.len == 0) {
        try cliques.append(clique.*);
        return;
    }
    defer clique.deinit();

    while (candidates.popOrNull()) |candidate| {
        var new_clique = try clique.clone();
        try new_clique.append(candidate);
        var new_candidates = NodeSet.init(clique.allocator);
        for (candidates.items) |other| {
            if (links.contains(.{ candidate, other }))
                try new_candidates.append(other);
        }
        var new_checked = NodeSet.init(clique.allocator);
        for (checked.items) |other| {
            if (links.contains(.{ candidate, other }))
                try new_checked.append(other);
        }
        bronKerbosch(&new_clique, &new_candidates, &new_checked, cliques) catch @panic("recur");
        try checked.append(candidate);
    }
}

fn solve_part2() !void {
    var cliques = std.ArrayList(NodeSet).init(nodes.allocator);
    defer {
        for (cliques.items) |*item|
            item.deinit();
        cliques.deinit();
    }
    var empty1 = NodeSet.init(nodes.allocator);
    var empty2 = NodeSet.init(nodes.allocator);
    var all = NodeSet.init(nodes.allocator);
    for (0..nodes.items.len) |idx|
        try all.append(idx);

    bronKerbosch(&empty1, &all, &empty2, &cliques) catch @panic("recur");

    var best: usize = 0;
    var best_size: usize = 0;
    for (cliques.items, 0..) |clique, idx|
        if (clique.items.len > best_size) {
            best_size = clique.items.len;
            best = idx;
        };

    var buf = std.ArrayList(Name).init(nodes.allocator);
    defer buf.deinit();
    for (cliques.items[best].items) |node|
        try buf.append(nodes.items[node].name);
    std.mem.sort(Name, buf.items, {}, nameLessThan);
    info("{s}", .{buf.items[0]});
    for (buf.items[1..]) |key|
        info(",{s}", .{key});
    info("\n", .{});
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
    links = @TypeOf(links).init(allocator);
    defer links.deinit();

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
        try links.put(.{ fid, sid }, void{});
        try links.put(.{ sid, fid }, void{});
        try nodes.items[fid].append(sid);
        try nodes.items[sid].append(fid);
    }

    try solve_part1();
    try solve_part2();
}
