pub fn word(hi: u8, lo: u8) u16 {
    return @as(u16, hi) << 8 | @as(u16, lo);
}

pub const Machine = struct {
    memory: [0x1000]u8,
    vram: [64 * 32]bool,
    regs: [16]u8,
    I: u16,
    sound_timer: u8,
    delay_timer: u8,
    pc: u16,
    sp: u8,
    stack: [0x100]u16,

    keyboard: [16]bool,

    op: Instruction,

    pub fn op_cls(self: *Machine) void {
        self.vram = [_]bool{false} ** self.vram.len;
    }

    pub fn op_ret(self: *Machine) void {
        self.sp -= 1;
        self.pc = self.stack[self.sp];
    }

    pub fn op_jp(self: *Machine) void {
        self.pc = self.op.nnn();
    }

    pub fn op_call(self: *Machine) void {
        self.stack[self.sp] = self.pc;
        self.sp += 1;
        self.pc = self.op.nnn();
    }

    pub fn op_sek(self: *Machine) void {
        const r = self.op.x();
        const kk = self.op.kk();
        if (self.regs[r] == kk) {
            self.pc += 2;
        }
    }

    pub fn op_snek(self: *Machine) void {
        const r = self.op.x();
        const kk = self.op.kk();
        if (self.regs[r] != kk) {
            self.pc += 2;
        }
    }

    pub fn op_se(self: *Machine) void {
        const rx = self.op.x();
        const ry = self.op.y();
        if (self.regs[rx] == self.regs[ry]) {
            self.pc += 2;
        }
    }

    pub fn op_ldk(self: *Machine) void {
        const r = self.op.x();
        const kk = self.op.kk();
        self.regs[r] = kk;
    }

    pub fn op_addk(self: *Machine) void {
        const r = self.op.x();
        const kk = self.op.kk();
        const res = @addWithOverflow(self.regs[r], kk);
        self.regs[r] = res[0];
    }

    // 8xxx ALU ops

    pub fn op_ld(self: *Machine, rx: u4, ry: u4) void {
        self.regs[rx] = self.regs[ry];
    }

    pub fn op_or(self: *Machine, rx: u4, ry: u4) void {
        self.regs[rx] |= self.regs[ry];
    }

    pub fn op_and(self: *Machine, rx: u4, ry: u4) void {
        self.regs[rx] &= self.regs[ry];
    }

    pub fn op_xor(self: *Machine, rx: u4, ry: u4) void {
        self.regs[rx] ^= self.regs[ry];
    }

    pub fn op_add(self: *Machine, rx: u4, ry: u4) void {
        const res = @addWithOverflow(self.regs[rx], self.regs[ry]);
        self.regs[rx] = res[0];
        self.regs[0xf] = res[1];
    }

    pub fn op_sub(self: *Machine, rx: u4, ry: u4) void {
        const res = @subWithOverflow(self.regs[rx], self.regs[ry]);
        self.regs[rx] = res[0];
        self.regs[0xf] = ~res[1];
    }

    pub fn op_shr(self: *Machine, rx: u4, ry: u4) void {
        self.regs[0xf] = self.regs[rx] & 1;
        self.regs[rx] = self.regs[rx] >> 1;
        _ = ry;
    }

    pub fn op_subn(self: *Machine, rx: u4, ry: u4) void {
        const res = @subWithOverflow(self.regs[ry], self.regs[rx]);
        self.regs[rx] = res[0];
        self.regs[0xf] = ~res[1];
    }

    pub fn op_shl(self: *Machine, rx: u4, ry: u4) void {
        self.regs[0xf] = self.regs[rx] >> 7;
        self.regs[rx] = self.regs[rx] << 1;
        _ = ry;
    }

    pub fn op_sne(self: *Machine) void {
        const rx = self.op.x();
        const ry = self.op.y();
        if (self.regs[rx] != self.regs[ry]) {
            self.pc += 2;
        }
    }

    pub fn op_ldi(self: *Machine) void {
        self.I = self.op.nnn();
    }

    pub fn op_jpr(self: *Machine) void {
        const v0: u8 = self.regs[0];
        self.pc += v0;
    }

    pub fn op_rnd(self: *Machine) void {
        // TODO: generate random number
        const rng: u8 = 0xAA;
        const rx = self.op.x();
        self.regs[rx] &= rng;
    }

    // TODO: draw

    pub fn op_skp(self: *Machine) void {
        const rx: u4 = @intCast(self.regs[self.op.x()]);
        if (self.keyboard[rx]) {
            self.pc += 2;
        }
    }

    pub fn op_sknp(self: *Machine) void {
        const rx: u4 = @intCast(self.regs[self.op.x()]);
        if (!self.keyboard[rx]) {
            self.pc += 2;
        }
    }

    pub fn op_ld_vx_dt(self: *Machine) void {
        self.regs[self.op.x()] = self.delay_timer;
    }

    pub fn op_ld_vx_k(self: *Machine) void {
        for (self.keyboard, 0..) |pressed, key| {
            if (pressed) {
                self.regs[self.op.x()] = key;
                break;
            }
        } else {
            self.pc -= 2;
        }
    }

    pub fn op_ld_dt_vx(self: *Machine) void {
        self.delay_timer = self.regs[self.op.x()];
    }

    pub fn op_ld_st_vx(self: *Machine) void {
        self.sound_timer = self.regs[self.op.x()];
    }

    pub fn op_add_i_vx(self: *Machine) void {
        self.I +%= self.regs[self.op.x()];
    }

    // TODO:
    //pub fn op_ld_f_vx

    // BCD representation
    pub fn op_ld_b_vx(self: *Machine) void {
        const vx: u8 = self.regs[self.op.x()];
        const ones: u8 = vx % 10;
        const tens: u8 = vx / 10 % 10;
        const hundreds: u8 = vx / 100 % 10;
        self.memory[self.I] = hundreds;
        self.memory[self.I + 1] = tens;
        self.memory[self.I + 2] = ones;
    }

    pub fn op_push(self: *Machine) void {
        for (0..self.op.x()) |i| {
            self.memory[self.I + i] = self.regs[i];
        }
    }

    pub fn op_pop(self: *Machine) void {
        for (0..self.op.x()) |i| {
            self.regs[i] = self.memory[self.I + i];
        }
    }

    pub fn fetch(self: *Machine) u8 {
        const data = self.memory[self.pc];
        self.pc += 1;
        return data;
    }

    pub fn fetch16(self: *Machine) u16 {
        const hi = self.memory[self.pc];
        const lo = self.memory[self.pc + 1];
        self.pc += 2;
        return word(hi, lo);
    }

    pub fn step(self: *Machine) error{UnknownOp}!void {
        const hi = self.fetch();
        const lo = self.fetch();
        const op = Instruction{ .op = word(hi, lo) };
        self.op = op;

        switch (op.z()) {
            0x0 => switch (self.op.n) {
                0xE0 => self.op_cls(),
                0xEE => self.op_ret(),
                else => return error.UnknownOp,
            },
            0x1 => self.op_jp(),
            0x2 => self.op_call(),
            0x3 => self.op_sek(),
            0x4 => self.op_snek(),
            0x5 => self.op_se(),
            0x6 => self.op_ldk(),
            0x7 => self.op_addk(),
            0x8 => {
                const rx = self.op.x();
                const ry = self.op.y();
                switch (self.op.n) {
                    0x0 => self.op_ld(rx, ry),
                    0x1 => self.op_or(rx, ry),
                    0x2 => self.op_and(rx, ry),
                    0x3 => self.op_xor(rx, ry),
                    0x4 => self.op_add(rx, ry),
                    0x5 => self.op_sub(rx, ry),
                    0x6 => self.op_shr(rx, ry),
                    0x7 => self.op_subn(rx, ry),
                    0xE => self.op_shl(rx, ry),
                    else => return error.UnknownOp,
                }
            },
            0x9 => self.op_sne(),
            0xA => self.op_ldi(),
            0xB => self.op_jpr(),
            0xC => self.op_rnd(),
            //0xD TODO
            0xE => switch (self.op.n) {
                0x9E => self.op_skp(),
                0xA1 => self.op_sknp(),
                else => return error.UnknownOp,
            },
            0xF => switch (self.op.n) {
                0x07 => self.op_ld_vx_dt(),
                0x0A => self.op_ld_vx_k(),
                0x15 => self.op_ld_dt_vx(),
                0x18 => self.op_ld_st_vx(),
                0x1E => self.op_add_i_vx(),
                //0x29 TODO
                0x33 => self.op_ld_b_vx(),
                0x55 => self.op_push(),
                0x65 => self.op_pop(),
                else => return error.UnknownOp,
            },
            else => return error.UnknownOp,
        }
    }
};

pub const Instruction = struct {
    op: u16,

    pub fn nnn(self: Instruction) u12 {
        return @intCast(self.op);
    }

    pub fn kk(self: Instruction) u8 {
        return @intCast(self.op);
    }

    pub fn z(self: Instruction) u4 {
        return @intCast(self.hi >> 12);
    }

    pub fn x(self: Instruction) u4 {
        return @intCast(self.hi >> 8);
    }

    pub fn y(self: Instruction) u4 {
        return @intCast(self.lo >> 4);
    }

    pub fn n(self: Instruction) u4 {
        return @intCast(self.lo);
    }
};
