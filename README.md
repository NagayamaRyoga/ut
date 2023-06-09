# UT

Simple header only Unit Testing Framework for C99

## Examples

```c
#define UT_MAIN
#include "ut.h"

int factorial(int n) {
    if (n > 0) {
        return n * factorial(n - 1);
    }
    return 1;
}

TEST(factorial) {
    EXPECT(factorial(0), eq(1));
    EXPECT(factorial(1), eq(1));
    EXPECT(factorial(5), eq(120), "5! == 120 (actual %d)", ACTUAL);
}

TEST(string) {
    const char *s = "Hello, world!";

    ASSERT(s, is_not_null);
    ASSERT(s, not(is_null));
    EXPECT(s, eq_str("Hello, world!"));
    EXPECT((s, 4), eq_str_n("Hell"));
    EXPECT(s, contains("world"));
    EXPECT(s, not(contains("nya")));
}

int main(void) {
    return RUN_ALL();
}
```

See [example.c](example.c).

## Usage

### via CMake

```cmake
include(FetchContent)

FetchContent_Declare(ut
    GIT_REPOSITORY "https://github.com/NagayamaRyoga/ut.git"
    GIT_TAG        "main"
)

FetchContent_MakeAvailable(ut)

add_executable(your_project your_src.c)
target_link_library(your_project ut)
```
