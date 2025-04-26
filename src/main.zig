//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.

const cpu = @import("cpu.zig");
const Machine = cpu.Machine;

pub fn loadRom(path: []const u8, machine: *Machine) !void {
    const file = try std.fs.cwd().openFile(
        path,
        .{ .mode = .read_only },
    );
    defer file.close();
    _ = try file.readAll(machine.memory[0x200..]);
    std.debug.print("Loaded ROM: {s}", .{path});
}

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    var arg_iter = try std.process.argsWithAllocator(std.heap.page_allocator);
    defer arg_iter.deinit();

    var machine: Machine = undefined;
    var loaded_rom = false;

    while (arg_iter.next()) |arg| {
        std.debug.print("Found arg: {s}\n", .{arg});

        if (std.mem.eql(u8, arg, "-i")) {
            if (arg_iter.next()) |input_path| {
                try loadRom(input_path, &machine);
            }
            loaded_rom = true;
        } else {
            std.debug.print("Unknown argument: {s}\n", .{arg});
        }
    }

    if (!loaded_rom) {
        std.debug.print("No ROM loaded, exiting.\n", .{});
        return error.NoRomLoaded;
    }

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // Don't forget to flush!
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "use other module" {
    try std.testing.expectEqual(@as(i32, 150), lib.add(100, 50));
}

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

const std = @import("std");

/// This imports the separate module containing `root.zig`. Take a look in `build.zig` for details.
const lib = @import("zigchip8_lib");
