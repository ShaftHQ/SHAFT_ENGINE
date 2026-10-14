package com.shaft.validation.internal;

import java.io.IOException;
import java.io.InputStream;
import java.lang.StackWalker.Option;
import java.lang.StackWalker.StackFrame;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

/**
 * A terminal such as {@code isEqualTo} evaluates immediately when its result is discarded.
 * The same statement may still call {@code withCustomReportMessage} or {@code perform}
 * on that result; those calls must see the custom message, so evaluation waits for them.
 */
final class ValidationChain {
    private static final int INVOKEVIRTUAL = 0xb6;
    private static final int INVOKESPECIAL = 0xb7;
    private static final int INVOKESTATIC = 0xb8;
    private static final int INVOKEINTERFACE = 0xb9;
    private static final int INVOKEDYNAMIC = 0xba;
    private static final int[] POP = new int[256];
    private static final int[] PUSH = new int[256];
    private static final int[] LEN = new int[256];
    private static final ClassValue<Parsed> CLASSES = new ClassValue<>() {
        @Override
        protected Parsed computeValue(Class<?> type) {
            return Parsed.read(type);
        }
    };

    static {
        op(0x00, 0, 0, 1);
        op(0x01, 0, 1, 1);
        range(0x02, 0x08, 0, 1, 1);
        range(0x09, 0x0a, 0, 2, 1);
        range(0x0b, 0x0d, 0, 1, 1);
        range(0x0e, 0x0f, 0, 2, 1);
        op(0x10, 0, 1, 2);
        op(0x11, 0, 1, 3);
        op(0x12, 0, 1, 2);
        op(0x13, 0, 1, 3);
        op(0x14, 0, 2, 3);
        op(0x15, 0, 1, 2);
        op(0x16, 0, 2, 2);
        op(0x17, 0, 1, 2);
        op(0x18, 0, 2, 2);
        op(0x19, 0, 1, 2);
        range(0x1a, 0x1d, 0, 1, 1);
        range(0x1e, 0x21, 0, 2, 1);
        range(0x22, 0x25, 0, 1, 1);
        range(0x26, 0x29, 0, 2, 1);
        range(0x2a, 0x2d, 0, 1, 1);
        op(0x2e, 2, 1, 1);
        op(0x2f, 2, 2, 1);
        op(0x30, 2, 1, 1);
        op(0x31, 2, 2, 1);
        op(0x32, 2, 1, 1);
        op(0x33, 2, 1, 1);
        op(0x34, 2, 1, 1);
        op(0x35, 2, 1, 1);
        op(0x36, 1, 0, 2);
        op(0x37, 2, 0, 2);
        op(0x38, 1, 0, 2);
        op(0x39, 2, 0, 2);
        op(0x3a, 1, 0, 2);
        range(0x3b, 0x3e, 1, 0, 1);
        range(0x3f, 0x42, 2, 0, 1);
        range(0x43, 0x46, 1, 0, 1);
        range(0x47, 0x4a, 2, 0, 1);
        range(0x4b, 0x4e, 1, 0, 1);
        op(0x4f, 3, 0, 1);
        op(0x50, 4, 0, 1);
        op(0x51, 3, 0, 1);
        op(0x52, 4, 0, 1);
        op(0x53, 3, 0, 1);
        op(0x54, 3, 0, 1);
        op(0x55, 3, 0, 1);
        op(0x56, 3, 0, 1);
        op(0x57, 1, 0, 1);
        op(0x58, 2, 0, 1);
        op(0x59, 1, 2, 1);
        op(0x5a, 2, 3, 1);
        op(0x5b, 3, 4, 1);
        op(0x5c, 2, 4, 1);
        op(0x5d, 3, 5, 1);
        op(0x5e, 4, 6, 1);
        op(0x5f, 2, 2, 1);
        binary(0x60);
        binary(0x64);
        binary(0x68);
        binary(0x6c);
        binary(0x70);
        op(0x74, 1, 1, 1);
        op(0x75, 2, 2, 1);
        op(0x76, 1, 1, 1);
        op(0x77, 2, 2, 1);
        op(0x78, 2, 1, 1);
        op(0x79, 3, 2, 1);
        op(0x7a, 2, 1, 1);
        op(0x7b, 3, 2, 1);
        op(0x7c, 2, 1, 1);
        op(0x7d, 3, 2, 1);
        binary(0x7e);
        binary(0x82);
        op(0x84, 0, 0, 3);
        op(0x85, 1, 2, 1);
        op(0x86, 1, 1, 1);
        op(0x87, 1, 2, 1);
        op(0x88, 2, 1, 1);
        op(0x89, 2, 1, 1);
        op(0x8a, 2, 2, 1);
        op(0x8b, 1, 1, 1);
        op(0x8c, 1, 2, 1);
        op(0x8d, 1, 2, 1);
        op(0x8e, 2, 1, 1);
        op(0x8f, 2, 2, 1);
        op(0x90, 2, 1, 1);
        op(0x91, 1, 1, 1);
        op(0x92, 1, 1, 1);
        op(0x93, 1, 1, 1);
        op(0x94, 4, 1, 1);
        op(0x95, 2, 1, 1);
        op(0x96, 2, 1, 1);
        op(0x97, 4, 1, 1);
        op(0x98, 4, 1, 1);
        op(0xb2, 0, 1, 3);
        op(0xb3, 1, 0, 3);
        op(0xb4, 1, 1, 3);
        op(0xb5, 2, 0, 3);
        op(0xbb, 0, 1, 3);
        op(0xbc, 1, 1, 2);
        op(0xbd, 1, 1, 3);
        op(0xbe, 1, 1, 1);
        op(0xc0, 1, 1, 3);
        op(0xc1, 1, 1, 3);
    }

    private ValidationChain() {
    }

    static boolean callerContinues() {
        try {
            return StackWalker.getInstance(Option.RETAIN_CLASS_REFERENCE)
                    .walk(stream -> {
                        String invoked = null;
                        for (StackFrame frame : stream.limit(12).toList()) {
                            if (internalFrame(frame.getClassName())) {
                                invoked = frame.getMethodName();
                                continue;
                            }
                            return resultIsChained(frame, invoked);
                        }
                        return false;
                    });
        } catch (RuntimeException ignored) {
            return false;
        }
    }

    private static boolean internalFrame(String className) {
        return className.endsWith("ValidationsBuilder")
                || className.endsWith("ValidationsExecutor")
                || className.endsWith("ValidationChain");
    }

    private static boolean resultIsChained(StackFrame frame, String invokedName) {
        if (invokedName == null || frame.getMethodName() == null || frame.getDescriptor() == null || frame.getLineNumber() < 0) {
            return false;
        }
        Parsed parsed = CLASSES.get(frame.getDeclaringClass());
        MethodBody body = parsed.body(frame.getMethodName(), frame.getDescriptor());
        if (body == null) {
            return false;
        }
        int matches = 0;
        boolean allChained = true;
        int pc = 0;
        while (pc >= 0 && pc < body.code.length) {
            int opcode = body.code[pc] & 0xff;
            if (opcode >= INVOKEVIRTUAL && opcode <= INVOKEDYNAMIC && body.lineAt(pc) == frame.getLineNumber()) {
                int index = ((body.code[pc + 1] & 0xff) << 8) | (body.code[pc + 2] & 0xff);
                if (invokedName.equals(parsed.name(index))) {
                    matches++;
                    allChained &= chainedAfter(parsed, body.code, pc);
                }
            }
            pc = nextPc(body.code, pc);
        }
        return matches > 0 && allChained;
    }

    private static boolean chainedAfter(Parsed parsed, byte[] code, int bci) {
        int width = instructionWidth(code, bci);
        if (width <= 0) {
            return false;
        }
        int pc = bci + width;
        int depth = 1;
        int limit = Math.min(code.length, pc + 256);
        while (pc < limit && depth >= 1) {
            int opcode = code[pc] & 0xff;
            if (opcode >= INVOKEVIRTUAL && opcode <= INVOKEDYNAMIC) {
                int index = ((code[pc + 1] & 0xff) << 8) | (code[pc + 2] & 0xff);
                String descriptor = parsed.descriptor(index);
                if (descriptor == null) {
                    return false;
                }
                int arguments = argumentSlots(descriptor);
                if (arguments < 0) {
                    return false;
                }
                boolean receiver = opcode != INVOKESTATIC && opcode != INVOKEDYNAMIC;
                int pop = arguments + (receiver ? 1 : 0);
                if (receiver && depth == pop && chainMethod(parsed.name(index))) {
                    return true;
                }
                if (pop > depth) {
                    return false;
                }
                depth = depth - pop + returnSlots(descriptor);
                if (depth < 1) {
                    return false;
                }
                pc += opcode == INVOKEINTERFACE || opcode == INVOKEDYNAMIC ? 5 : 3;
                continue;
            }
            int length = LEN[opcode];
            if (length <= 0) {
                return false;
            }
            int pop = POP[opcode];
            if (pop > depth) {
                return false;
            }
            depth = depth - pop + PUSH[opcode];
            if (depth < 1) {
                return false;
            }
            pc += length;
        }
        return false;
    }

    private static int instructionWidth(byte[] code, int bci) {
        int opcode = code[bci] & 0xff;
        if (opcode >= INVOKEVIRTUAL && opcode <= INVOKEDYNAMIC) {
            return opcode == INVOKEINTERFACE || opcode == INVOKEDYNAMIC ? 5 : 3;
        }
        return LEN[opcode];
    }

    private static int nextPc(byte[] code, int pc) {
        int opcode = code[pc] & 0xff;
        try {
            if (opcode == 0xaa) {
                int index = (pc + 4) & ~3;
                int low = readInt(code, index + 4);
                int high = readInt(code, index + 8);
                return index + 12 + (high - low + 1) * 4;
            }
            if (opcode == 0xab) {
                int index = (pc + 4) & ~3;
                int pairs = readInt(code, index + 4);
                return index + 8 + pairs * 8;
            }
            if (opcode == 0xc4) {
                return pc + ((code[pc + 1] & 0xff) == 0x84 ? 6 : 4);
            }
        } catch (RuntimeException ignored) {
            return -1;
        }
        int width = instructionWidth(code, pc);
        if (width <= 0) {
            width = switch (opcode) {
                case 0xa7, 0xa8, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xc6, 0xc7 -> 3;
                case 0xa9 -> 2;
                case 0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xbf, 0xc2, 0xc3 -> 1;
                case 0xc5 -> 4;
                case 0xc8, 0xc9 -> 5;
                default -> 0;
            };
        }
        return width <= 0 || pc + width > code.length ? -1 : pc + width;
    }

    private static int readInt(byte[] code, int offset) {
        return ((code[offset] & 0xff) << 24)
                | ((code[offset + 1] & 0xff) << 16)
                | ((code[offset + 2] & 0xff) << 8)
                | (code[offset + 3] & 0xff);
    }

    private static boolean chainMethod(String name) {
        return "withCustomReportMessage".equals(name) || "perform".equals(name);
    }

    private static int argumentSlots(String descriptor) {
        int slots = 0;
        int index = 1;
        while (index < descriptor.length() && descriptor.charAt(index) != ')') {
            char type = descriptor.charAt(index);
            if (type == 'J' || type == 'D') {
                slots += 2;
                index++;
            } else if (type == 'L') {
                slots++;
                index = descriptor.indexOf(';', index);
                if (index < 0) {
                    return -1;
                }
                index++;
            } else if (type == '[') {
                slots++;
                while (index < descriptor.length() && descriptor.charAt(index) == '[') {
                    index++;
                }
                if (index >= descriptor.length()) {
                    return -1;
                }
                if (descriptor.charAt(index) == 'L') {
                    index = descriptor.indexOf(';', index);
                    if (index < 0) {
                        return -1;
                    }
                }
                index++;
            } else {
                slots++;
                index++;
            }
        }
        return slots;
    }

    private static int returnSlots(String descriptor) {
        int index = descriptor.indexOf(')');
        if (index < 0 || index + 1 >= descriptor.length()) {
            return 0;
        }
        char type = descriptor.charAt(index + 1);
        if (type == 'V') {
            return 0;
        }
        return type == 'J' || type == 'D' ? 2 : 1;
    }

    private static void binary(int start) {
        op(start, 2, 1, 1);
        op(start + 1, 4, 2, 1);
        op(start + 2, 2, 1, 1);
        op(start + 3, 4, 2, 1);
    }

    private static void range(int from, int to, int pop, int push, int length) {
        for (int opcode = from; opcode <= to; opcode++) {
            op(opcode, pop, push, length);
        }
    }

    private static void op(int opcode, int pop, int push, int length) {
        POP[opcode] = pop;
        PUSH[opcode] = push;
        LEN[opcode] = length;
    }

    private record MethodBody(byte[] code, int[] linePc, int[] lineNo) {
        int lineAt(int bci) {
            int line = -1;
            int best = -1;
            for (int index = 0; index < linePc.length; index++) {
                if (linePc[index] <= bci && linePc[index] >= best) {
                    best = linePc[index];
                    line = lineNo[index];
                }
            }
            return line;
        }
    }

    private record Parsed(String[] utf8, int[] refName, int[] refType, Map<String, MethodBody> methods) {
        private static final Parsed EMPTY = new Parsed(new String[1], new int[1], new int[1], Map.of());

        static Parsed read(Class<?> type) {
            try {
                byte[] bytes = classBytes(type);
                if (bytes == null) {
                    return EMPTY;
                }
                return parse(bytes);
            } catch (RuntimeException | IOException ignored) {
                return EMPTY;
            }
        }

        MethodBody body(String name, String descriptor) {
            return methods.get(name + descriptor);
        }

        String name(int index) {
            if (index <= 0 || index >= refName.length) {
                return null;
            }
            int utf = refName[index];
            return utf > 0 && utf < utf8.length ? utf8[utf] : null;
        }

        String descriptor(int index) {
            if (index <= 0 || index >= refType.length) {
                return null;
            }
            int utf = refType[index];
            return utf > 0 && utf < utf8.length ? utf8[utf] : null;
        }

        private static byte[] classBytes(Class<?> type) throws IOException {
            String resource = type.getName().replace('.', '/') + ".class";
            ClassLoader loader = type.getClassLoader();
            InputStream input = loader == null
                    ? ClassLoader.getSystemResourceAsStream(resource)
                    : loader.getResourceAsStream(resource);
            if (input == null) {
                return null;
            }
            try (input) {
                return input.readAllBytes();
            }
        }

        private static Parsed parse(byte[] bytes) {
            ByteBuffer buffer = ByteBuffer.wrap(bytes);
            if (buffer.getInt() != 0xCAFEBABE) {
                return EMPTY;
            }
            buffer.getChar();
            buffer.getChar();
            int count = buffer.getChar();
            String[] utf8 = new String[count];
            int[] nameAndTypeName = new int[count];
            int[] nameAndTypeDescriptor = new int[count];
            int[] refNameAndType = new int[count];
            for (int index = 1; index < count; index++) {
                int tag = buffer.get() & 0xff;
                switch (tag) {
                    case 1 -> {
                        int length = buffer.getChar();
                        byte[] data = new byte[length];
                        buffer.get(data);
                        utf8[index] = new String(data, StandardCharsets.UTF_8);
                    }
                    case 7, 8, 16, 19, 20 -> buffer.getChar();
                    case 3, 4 -> buffer.getInt();
                    case 5, 6 -> {
                        buffer.getLong();
                        index++;
                    }
                    case 9, 10, 11 -> {
                        buffer.getChar();
                        refNameAndType[index] = buffer.getChar();
                    }
                    case 12 -> {
                        nameAndTypeName[index] = buffer.getChar();
                        nameAndTypeDescriptor[index] = buffer.getChar();
                    }
                    case 15 -> {
                        buffer.get();
                        buffer.getChar();
                    }
                    case 17, 18 -> {
                        buffer.getChar();
                        refNameAndType[index] = buffer.getChar();
                    }
                    default -> {
                        return EMPTY;
                    }
                }
            }
            int[] refName = new int[count];
            int[] refType = new int[count];
            for (int index = 1; index < count; index++) {
                int nameAndType = refNameAndType[index];
                if (nameAndType > 0 && nameAndType < count) {
                    refName[index] = nameAndTypeName[nameAndType];
                    refType[index] = nameAndTypeDescriptor[nameAndType];
                }
            }
            buffer.getChar();
            buffer.getChar();
            buffer.getChar();
            int interfaces = buffer.getChar();
            buffer.position(buffer.position() + interfaces * 2);
            int fields = buffer.getChar();
            for (int field = 0; field < fields; field++) {
                skipMember(buffer);
            }
            int methodCount = buffer.getChar();
            Map<String, MethodBody> methods = new HashMap<>();
            for (int method = 0; method < methodCount; method++) {
                readMethod(buffer, utf8, methods);
            }
            return new Parsed(utf8, refName, refType, methods);
        }

        private static void skipMember(ByteBuffer buffer) {
            buffer.getChar();
            buffer.getChar();
            buffer.getChar();
            skipAttributes(buffer);
        }

        private static void readMethod(ByteBuffer buffer, String[] utf8, Map<String, MethodBody> methods) {
            buffer.getChar();
            int name = buffer.getChar();
            int descriptor = buffer.getChar();
            int attributes = buffer.getChar();
            byte[] methodCode = null;
            int[] linePc = new int[0];
            int[] lineNo = new int[0];
            for (int attribute = 0; attribute < attributes; attribute++) {
                int attributeName = buffer.getChar();
                int length = buffer.getInt();
                int start = buffer.position();
                if (methodCode == null && attributeName < utf8.length && "Code".equals(utf8[attributeName]) && length >= 8) {
                    buffer.getChar();
                    buffer.getChar();
                    int codeLength = buffer.getInt();
                    if (codeLength >= 0 && buffer.remaining() >= codeLength) {
                        methodCode = new byte[codeLength];
                        buffer.get(methodCode);
                        int exceptions = buffer.getChar();
                        buffer.position(buffer.position() + exceptions * 8);
                        int nested = buffer.getChar();
                        for (int child = 0; child < nested; child++) {
                            int childName = buffer.getChar();
                            int childLength = buffer.getInt();
                            int childStart = buffer.position();
                            if (childName < utf8.length && "LineNumberTable".equals(utf8[childName]) && childLength >= 2) {
                                int rows = buffer.getChar();
                                linePc = new int[rows];
                                lineNo = new int[rows];
                                for (int row = 0; row < rows; row++) {
                                    linePc[row] = buffer.getChar();
                                    lineNo[row] = buffer.getChar();
                                }
                            }
                            buffer.position(childStart + childLength);
                        }
                    }
                }
                buffer.position(start + length);
            }
            if (methodCode != null && name < utf8.length && descriptor < utf8.length) {
                methods.put(utf8[name] + utf8[descriptor], new MethodBody(methodCode, linePc, lineNo));
            }
        }

        private static void skipAttributes(ByteBuffer buffer) {
            int attributes = buffer.getChar();
            for (int attribute = 0; attribute < attributes; attribute++) {
                buffer.getChar();
                int length = buffer.getInt();
                buffer.position(buffer.position() + length);
            }
        }
    }
}
