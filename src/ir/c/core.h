#ifndef __AURA_CORE_H__
#define __AURA_CORE_H__

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>

typedef bool Bool;
typedef int32_t Int;
typedef uint32_t UInt;
typedef int8_t Int8;
typedef int16_t Int16;
typedef int32_t Int32;
typedef int64_t Int64;
typedef uint8_t UInt8;
typedef uint16_t UInt16;
typedef uint32_t UInt32;
typedef uint64_t UInt64;
typedef float Float;
typedef float Float32;
typedef double Float64;
typedef char Char;
typedef const char* String;
typedef uint64_t Atom;
typedef void Void;

void print(String str) {
    printf("%s", str);
}

#endif // __AURA_CORE_H__
