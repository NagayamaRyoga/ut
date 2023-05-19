#ifndef INCLUDE_UT_H
#define INCLUDE_UT_H

#ifndef __STDC_WANT_LIB_EXT2__
#    define __STDC_WANT_LIB_EXT2__ 1
#endif // __STDC_WANT_LIB_EXT2__

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef __GNUC__
#    define UT_GNU_ATTR(attr) __attribute__(attr)
#else
#    define UT_GNU_ATTR(attr)
#endif // __GNUC__

#ifndef UT_NO_ALIASES
#    define TEST UT_TEST
#    define REGISTER UT_REGISTER
#    define BEFORE_EACH UT_BEFORE_EACH
#    define AFTER_EACH UT_AFTER_EACH
#    define SUBTEST UT_SUBTEST
#    define EXPECT UT_EXPECT
#    define ASSERT UT_ASSERT
#    define FAIL UT_FAIL
#    define ACTUAL UT_ACTUAL
#    define RUN_ALL UT_RUN_ALL
#endif // UT_NO_ALIASES

#ifndef UT_VERBOSE
#    define UT_VERBOSE 0
#endif // UT_VERBOSE

typedef struct ut_test_def_t ut_test_def_t;
typedef struct ut_test_list_t ut_test_list_t;
typedef struct ut_result_t ut_result_t;
typedef struct ut_tester_t ut_tester_t;
typedef struct ut_str_span_t ut_str_span_t;

typedef void ut_test_func_t(ut_tester_t *);
typedef void ut_setup_func_t(void);
typedef void ut_teardown_func_t(void);

struct ut_test_def_t {
    ut_test_def_t *next;
    const char *testname;
    const char *file;
    unsigned long long line;
    ut_test_func_t *run;
};

struct ut_test_list_t {
    ut_test_def_t *head, **tail;
    ut_setup_func_t *setup;
    ut_teardown_func_t *teardown;
};

typedef enum ut_result_type_t {
    ut_result_type_subtest,
    ut_result_type_assert,
} ut_result_type_t;

struct ut_result_t {
    ut_result_type_t type;
    ut_result_t *parent;
    ut_result_t *next;
    const char *file;
    unsigned long long line;
    char *message;

    union {
        struct {
            ut_result_t *head;
            clock_t start_time, end_time;
            int total, ok;
        } subtest;

        struct {
            const char *expr;
            bool ok;
        } assert;
    } _;
};

struct ut_tester_t {
    ut_result_t *current;
    ut_result_t **tail;
};

struct ut_str_span_t {
    const char *ptr;
    size_t len;
};

extern ut_test_list_t ut_test_list;

static inline void ut_register(ut_test_def_t *test);
static inline void ut_before_each(ut_setup_func_t *setup);
static inline void ut_after_each(ut_teardown_func_t *teardown);

static inline ut_result_t *ut_result_new_subtest(ut_result_t *parent, const char *file, unsigned long long line, const char *format, ...) UT_GNU_ATTR((format(printf, 4, 5)));
static inline ut_result_t *ut_result_new_subtest_v(ut_result_t *parent, const char *file, unsigned long long line, const char *format, va_list args);
static inline ut_result_t *ut_result_new_assert_v(ut_result_t *parent, const char *file, unsigned long long line, const char *expr, bool ok, const char *format, va_list args);
static inline void ut_result_delete(ut_result_t *r);
static inline void ut_result_subtest_start(ut_result_t *r);
static inline void ut_result_subtest_finish(ut_result_t *r);
static inline double ut_result_subtest_duration(const ut_result_t *r);
static inline int ut_result_ok(const ut_result_t *r);
static inline void ut_result_dump_i(const ut_result_t *r, FILE *out, unsigned int depth);
static inline void ut_result_dump(const ut_result_t *r, FILE *out);

static inline void ut_tester_init(ut_tester_t *t, ut_result_t *subtest);
static inline ut_tester_t *ut_tester_subtest_start(ut_tester_t *t, ut_tester_t *subtester, const char *file, unsigned long long line, const char *format, ...) UT_GNU_ATTR((format(printf, 5, 6)));
static inline void ut_tester_subtest_finish(ut_tester_t *subtester);
static inline void ut_tester_push_result(ut_tester_t *t, ut_result_t *result);
static inline void ut_tester_expect(ut_tester_t *t, const char *file, unsigned long long line, const char *expr, bool ok, const char *format, ...) UT_GNU_ATTR((format(printf, 6, 7)));
static inline bool ut_tester_assert(ut_tester_t *t, const char *file, unsigned long long line, const char *expr, bool ok, const char *format, ...) UT_GNU_ATTR((format(printf, 6, 7)));
static inline void ut_tester_fail(ut_tester_t *t, const char *file, unsigned long long line, const char *format, ...) UT_GNU_ATTR((format(printf, 4, 5)));

#define UT_TESTER _ut_tester
#define UT_ACTUAL _0
#define UT_TEST_FUNC(tname) ut_test_##tname##_run
#define UT_TEST_INIT(tname) ut_test_##tname##_init
#define UT_TEST_DATA(tname) ut_test_##tname##_data

#define UT_TEST(tname) UT_TEST_I(UT_TESTER, tname)
#define UT_TEST_I(t, tname)                                                                                                                                                                            \
    static void UT_TEST_FUNC(tname)(ut_tester_t * (t));                                                                                                                                                \
    static void UT_TEST_INIT(tname)(void) UT_GNU_ATTR((constructor));                                                                                                                                  \
    static ut_test_def_t UT_TEST_DATA(tname) = {                                                                                                                                                       \
        .next = NULL,                                                                                                                                                                                  \
        .testname = #tname,                                                                                                                                                                            \
        .file = __FILE__,                                                                                                                                                                              \
        .line = __LINE__,                                                                                                                                                                              \
        .run = &UT_TEST_FUNC(tname),                                                                                                                                                                   \
    };                                                                                                                                                                                                 \
    static void UT_TEST_INIT(tname)(void) {                                                                                                                                                            \
        UT_REGISTER(tname);                                                                                                                                                                            \
    }                                                                                                                                                                                                  \
    static void UT_TEST_FUNC(tname)(UT_GNU_ATTR((unused)) ut_tester_t * (t))

#define UT_REGISTER(tname) ut_register(&UT_TEST_DATA(tname));
#define UT_BEFORE_EACH(setup) ut_before_each((setup))
#define UT_AFTER_EACH(teardown) ut_after_each((teardown))

#define UT_SUBTEST(...) UT_SUBTEST_I(UT_TESTER, __VA_ARGS__)
#define UT_SUBTEST_I(t, ...)                                                                                                                                                                           \
    for (ut_tester_t *_ut_tester_saved = (t), _ut_tester_subtester, (*t) = ut_tester_subtest_start(_ut_tester_saved, &_ut_tester_subtester, __FILE__, __LINE__, __VA_ARGS__); (t);                     \
         ut_tester_subtest_finish(&_ut_tester_subtester), (t) = NULL)

#define UT_PP_NO_MSG(...) UT_PP_NO_MSG_I(__VA_ARGS__, )
#define UT_PP_NO_MSG_I(m, ...) UT_PP_NO_MSG_II(__VA_OPT__(0, ) 1, )
#define UT_PP_NO_MSG_II(x, ...) x

#define UT_PP_IF(c, x, y) UT_PP_IF_I(c, x, y)
#define UT_PP_IF_I(c, x, y) UT_PP_IF_##c(x, y)
#define UT_PP_IF_0(x, y) y
#define UT_PP_IF_1(x, y) x

#define UT_STR_SPAN(x) UT_STR_SPAN_I x
#define UT_STR_SPAN_I(p, n) ((ut_str_span_t){.ptr = p, .len = n})

#define UT_MATCHER_DESC(mt) UT_MATCHER_DESC_I mt
#define UT_MATCHER_DESC_I(desc, prepare, pred, data) desc
#define UT_MATCHER_PREPARE(mt) UT_MATCHER_PREPARE_I mt
#define UT_MATCHER_PREPARE_I(desc, prepare, pred, data) prepare
#define UT_MATCHER_PRED(mt) UT_MATCHER_PRED_I mt
#define UT_MATCHER_PRED_I(desc, prepare, pred, data) pred
#define UT_MATCHER_DATA(mt) UT_MATCHER_DATA_I mt
#define UT_MATCHER_DATA_I(desc, prepare, pred, data) data
#define UT_MATCHER_ARGS(t, actual, data) UT_MATCHER_ARGS_II(t, actual, UT_MATCHER_ARGS_I data)
#define UT_MATCHER_ARGS_I(...) __VA_ARGS__
#define UT_MATCHER_ARGS_II(t, actual, ...) (t, actual __VA_OPT__(, ) __VA_ARGS__)

#define UT_MATCHER_PREPARE_0(t, actual) const __typeof(actual) _0 = (actual);
#define UT_MATCHER_PREPARE_1(t, actual, a1)                                                                                                                                                            \
    const __typeof(actual) _0 = (actual);                                                                                                                                                              \
    const __typeof(a1) _1 = (a1);
#define UT_MATCHER_PREPARE_2(t, actual, a1, a2)                                                                                                                                                        \
    const __typeof(actual) _0 = (actual);                                                                                                                                                              \
    const __typeof(a1) _1 = (a1);                                                                                                                                                                      \
    const __typeof(a2) _2 = (a2);
#define UT_MATCHER_PREPARE_3(t, actual, a1, a2, a3)                                                                                                                                                    \
    const __typeof(actual) _0 = (actual);                                                                                                                                                              \
    const __typeof(a1) _1 = (a1);                                                                                                                                                                      \
    const __typeof(a2) _2 = (a2);                                                                                                                                                                      \
    const __typeof(a3) _3 = (a3);
#define UT_MATCHER_PREPARE_4(t, actual, a1, a2, a3, a4)                                                                                                                                                \
    const __typeof(actual) _0 = (actual);                                                                                                                                                              \
    const __typeof(a1) _1 = (a1);                                                                                                                                                                      \
    const __typeof(a2) _2 = (a2);                                                                                                                                                                      \
    const __typeof(a3) _3 = (a3);                                                                                                                                                                      \
    const __typeof(a4) _4 = (a4);
#define UT_MATCHER_PREPARE_STR_1(t, actual, a1)                                                                                                                                                        \
    const __typeof(actual) _0 = (actual);                                                                                                                                                              \
    const char *const _1 = (a1);
#define UT_MATCHER_PREPARE_STR_N_1(t, actual, a1)                                                                                                                                                      \
    const ut_str_span_t _0 = UT_STR_SPAN(actual);                                                                                                                                                      \
    const char *const _1 = (a1);

#define UT_MATCH(t, f, actual, ...) UT_PP_IF(UT_PP_NO_MSG(__VA_ARGS__), UT_MATCH_I, UT_MATCH_II)(t, f, actual, __VA_ARGS__)
#define UT_MATCH_I(t, f, actual, m) UT_MATCH_II(t, f, actual, m, NULL)
#define UT_MATCH_II(t, f, actual, m, ...) UT_MATCH_III(t, f, actual, UT_MATCHER_##m, __VA_ARGS__)
#define UT_MATCH_III(t, f, actual, mt, ...) UT_MATCH_IV(t, f, UT_MATCHER_DESC(mt), UT_MATCHER_PREPARE(mt), UT_MATCHER_PRED(mt), UT_MATCHER_ARGS(t, actual, UT_MATCHER_DATA(mt)), __VA_ARGS__)
#define UT_MATCH_IV(t, f, desc, prepare, pred, args, ...) f(t, desc args, prepare args, pred args, __VA_ARGS__)

#define UT_EXPECT(actual, ...) UT_EXPECT_I(UT_TESTER, actual, __VA_ARGS__)
#define UT_EXPECT_I(t, actual, ...) UT_MATCH(t, UT_EXPECT_II, actual, __VA_ARGS__)
#define UT_EXPECT_II(t, desc, prepare, pred, ...)                                                                                                                                                      \
    do {                                                                                                                                                                                               \
        prepare;                                                                                                                                                                                       \
        ut_tester_expect((t), __FILE__, __LINE__, (desc), (pred), __VA_ARGS__);                                                                                                                        \
    } while (0)

#define UT_ASSERT(actual, ...) UT_ASSERT_I(UT_TESTER, actual, __VA_ARGS__)
#define UT_ASSERT_I(t, actual, ...) UT_MATCH(t, UT_ASSERT_II, actual, __VA_ARGS__)
#define UT_ASSERT_II(t, desc, prepare, pred, ...)                                                                                                                                                      \
    do {                                                                                                                                                                                               \
        prepare;                                                                                                                                                                                       \
        if (!ut_tester_assert((t), __FILE__, __LINE__, (desc), (pred), __VA_ARGS__)) {                                                                                                                 \
            return;                                                                                                                                                                                    \
        }                                                                                                                                                                                              \
    } while (0)

#define UT_FAIL(...) UT_FAIL_I(UT_TESTER __VA_OPT__(, ) __VA_ARGS__)
#define UT_FAIL_I(...) UT_PP_IF(UT_PP_NO_MSG(__VA_ARGS__), UT_FAIL_II, UT_FAIL_III)(__VA_ARGS__)
#define UT_FAIL_II(t) UT_FAIL_III(t, NULL)
#define UT_FAIL_III(t, ...)                                                                                                                                                                            \
    do {                                                                                                                                                                                               \
        ut_tester_fail((t), __FILE__, __LINE__, __VA_ARGS__);                                                                                                                                          \
        return;                                                                                                                                                                                        \
    } while (0)

// matchers
// EXPECT(x, is_true)
#define UT_MATCHER_is_true (UT_MATCHER_is_true_DESC, UT_MATCHER_PREPARE_0, UT_MATCHER_is_true_PRED, ())
#define UT_MATCHER_is_true_DESC(t, actual) #actual " == true"
#define UT_MATCHER_is_true_PRED(t, actual) ut_matcher_is_true(t, _0)

static inline bool ut_matcher_is_true(ut_tester_t *t, bool actual) {
    (void)t;
    return actual;
}

// EXPECT(x, is_false)
#define UT_MATCHER_is_false (UT_MATCHER_is_false_DESC, UT_MATCHER_PREPARE_0, UT_MATCHER_is_false_PRED, ())
#define UT_MATCHER_is_false_DESC(t, actual) #actual " == false"
#define UT_MATCHER_is_false_PRED(t, actual) ut_matcher_is_false(t, _0)

static inline bool ut_matcher_is_false(ut_tester_t *t, bool actual) {
    (void)t;
    return !actual;
}

// EXPECT(x, is_null)
#define UT_MATCHER_is_null (UT_MATCHER_is_null_DESC, UT_MATCHER_PREPARE_0, UT_MATCHER_is_null_PRED, ())
#define UT_MATCHER_is_null_DESC(t, actual) #actual " == NULL"
#define UT_MATCHER_is_null_PRED(t, actual) (_0 == NULL)

// EXPECT(x, is_not_null)
#define UT_MATCHER_is_not_null (UT_MATCHER_is_not_null_DESC, UT_MATCHER_PREPARE_0, UT_MATCHER_is_not_null_PRED, ())
#define UT_MATCHER_is_not_null_DESC(t, actual) #actual " != NULL"
#define UT_MATCHER_is_not_null_PRED(t, actual) (_0 != NULL)

// EXPECT(actual, is(op, x))
#define UT_MATCHER_is(op, x) (UT_MATCHER_is_DESC, UT_MATCHER_is_PREPARE, UT_MATCHER_is_PRED, (op, x))
#define UT_MATCHER_is_DESC(t, actual, op, x) #actual " " #op " " #x
#define UT_MATCHER_is_PREPARE(t, actual, op, x)                                                                                                                                                        \
    const __typeof(actual) _0 = (actual);                                                                                                                                                              \
    const __typeof(x) _1 = (x);
#define UT_MATCHER_is_PRED(t, actual, op, x) ((_0)op(_1))

// EXPECT(actual, eq(expected))
#define UT_MATCHER_eq(expected) UT_MATCHER_is(==, expected)

// EXPECT(actual, ne(x))
#define UT_MATCHER_ne(x) UT_MATCHER_is(!=, x)

// EXPECT(actual, lt(x))
#define UT_MATCHER_lt(x) UT_MATCHER_is(<, x)

// EXPECT(actual, le(x))
#define UT_MATCHER_le(x) UT_MATCHER_is(<=, x)

// EXPECT(actual, gt(x))
#define UT_MATCHER_gt(x) UT_MATCHER_is(>, x)

// EXPECT(actual, ge(x))
#define UT_MATCHER_ge(x) UT_MATCHER_is(>=, x)

// EXPECT(actual, near(expected, eps))
#define UT_MATCHER_near(expected, eps) (UT_MATCHER_near_DESC, UT_MATCHER_PREPARE_2, UT_MATCHER_near_PRED, (expected, eps))
#define UT_MATCHER_near_DESC(t, actual, expected, eps) #actual " == " #expected " ± " #eps
#define UT_MATCHER_near_PRED(t, actual, expected, eps) ((_1 - _2) <= _0 && _0 <= (_1 + _2))

// EXPECT(actual, eq_str(x))
#define UT_MATCHER_eq_str(expected) (UT_MATCHER_eq_str_DESC, UT_MATCHER_PREPARE_STR_1, UT_MATCHER_eq_str_PRED, (expected))
#define UT_MATCHER_eq_str_DESC(t, actual, expected) #actual " == " #expected
#define UT_MATCHER_eq_str_PRED(t, actual, expected) ut_matcher_eq_str(t, _0, _1)

static inline bool ut_matcher_eq_str(ut_tester_t *t, const char *actual, const char *expected) {
    assert(expected != NULL);
    (void)t;

    if (actual == NULL) {
        return 0;
    }
    return strcmp(actual, expected) == 0;
}

// EXPECT((actual, actual_len), eq_str_n(expected))
#define UT_MATCHER_eq_str_n(expected) (UT_MATCHER_eq_str_n_DESC, UT_MATCHER_PREPARE_STR_N_1, UT_MATCHER_eq_str_n_PRED, (expected))
#define UT_MATCHER_eq_str_n_DESC(t, actual, expected) UT_MATCHER_eq_str_n_DESC_I actual " == " #expected
#define UT_MATCHER_eq_str_n_DESC_I(p, n) #p "[:" #n "]"
#define UT_MATCHER_eq_str_n_PRED(t, actual, expected) ut_matcher_eq_str_n(t, _0, _1)

static inline bool ut_matcher_eq_str_n(ut_tester_t *t, ut_str_span_t actual, const char *expected) {
    assert(expected != NULL);
    (void)t;

    if (actual.ptr == NULL) {
        return 0;
    }
    if (actual.len != strlen(expected)) {
        return 0;
    }
    return strncmp(actual.ptr, expected, actual.len) == 0;
}

// EXPECT(actual, contains(s))
#define UT_MATCHER_contains(s) (UT_MATCHER_contains_DESC, UT_MATCHER_PREPARE_STR_1, UT_MATCHER_contains_PRED, (s))
#define UT_MATCHER_contains_DESC(t, actual, s) #actual " contains " #s
#define UT_MATCHER_contains_PRED(t, actual, s) ut_matcher_contains(t, _0, _1)

static inline bool ut_matcher_contains(ut_tester_t *t, const char *actual, const char *s) {
    assert(s != NULL);
    (void)t;

    if (actual == NULL) {
        return 0;
    }
    return strstr(actual, s) != NULL;
}

// EXPECT(actual, not(matcher))
#define UT_MATCHER_not(matcher) UT_MATCHER_not_I(UT_MATCHER_##matcher)
#define UT_MATCHER_not_I(m) (UT_MATCHER_not_DESC, UT_MATCHER_not_PREPARE, UT_MATCHER_not_PRED, m)
#define UT_MATCHER_not_DESC(t, actual, desc, prepare, pred, data) "not " desc UT_MATCHER_ARGS(t, actual, data)
#define UT_MATCHER_not_PREPARE(t, actual, desc, prepare, pred, data) prepare UT_MATCHER_ARGS(t, actual, data)
#define UT_MATCHER_not_PRED(t, actual, desc, prepare, pred, data) (!(pred UT_MATCHER_ARGS(t, actual, data)))

#ifdef UT_MAIN
#    define UT_RUN_ALL() ut_run_all(__FILE__, __LINE__)
#endif // UT_MAIN

static inline void ut_register(ut_test_def_t *test) {
    if (ut_test_list.tail == NULL) {
        ut_test_list.tail = &ut_test_list.head;
    }

    test->next = NULL;
    *ut_test_list.tail = test;
    ut_test_list.tail = &test->next;
}

static inline void ut_before_each(ut_setup_func_t *setup) {
    ut_test_list.setup = setup;
}

static inline void ut_after_each(ut_teardown_func_t *teardown) {
    ut_test_list.teardown = teardown;
}

static inline ut_result_t *ut_result_new_subtest(ut_result_t *parent, const char *file, unsigned long long line, const char *format, ...) {
    va_list args;
    va_start(args, format);
    ut_result_t *r = ut_result_new_subtest_v(parent, file, line, format, args);
    va_end(args);
    return r;
}

static inline ut_result_t *ut_result_new_subtest_v(ut_result_t *parent, const char *file, unsigned long long line, const char *format, va_list args) {
    ut_result_t *r = (ut_result_t *)malloc(sizeof(ut_result_t));
    r->type = ut_result_type_subtest;
    r->parent = parent;
    r->next = NULL;
    r->file = file;
    r->line = line;
    r->message = NULL;
    r->_.subtest.head = NULL;
    r->_.subtest.start_time = 0;
    r->_.subtest.end_time = 0;
    r->_.subtest.total = -1;
    r->_.subtest.ok = -1;

    if (format != NULL) {
        if (vasprintf(&r->message, format, args) == -1) {
            r->message = NULL;
        }
    }

    return r;
}

static inline ut_result_t *ut_result_new_assert_v(ut_result_t *parent, const char *file, unsigned long long line, const char *expr, bool ok, const char *format, va_list args) {
    ut_result_t *r = (ut_result_t *)malloc(sizeof(ut_result_t));
    r->type = ut_result_type_assert;
    r->parent = parent;
    r->next = NULL;
    r->file = file;
    r->line = line;
    r->message = NULL;
    r->_.assert.expr = expr;
    r->_.assert.ok = ok;

    if (format != NULL) {
        if (vasprintf(&r->message, format, args) == -1) {
            r->message = NULL;
        }
    }

    return r;
}

static inline void ut_result_delete(ut_result_t *r) {
    while (r != NULL) {
        ut_result_t *next = r->next;

        switch (r->type) {
        case ut_result_type_subtest:
            ut_result_delete(r->_.subtest.head);
            break;
        case ut_result_type_assert:
            break;
        default:
            break;
        }

        free(r->message);
        free(r);

        r = next;
    }
}

static inline void ut_result_subtest_start(ut_result_t *r) {
    assert(r != NULL);
    assert(r->type == ut_result_type_subtest);

    r->_.subtest.end_time = r->_.subtest.start_time = clock();
}

static inline void ut_result_subtest_finish(ut_result_t *r) {
    assert(r != NULL);
    assert(r->type == ut_result_type_subtest);

    r->_.subtest.end_time = clock();

    r->_.subtest.total = 0;
    r->_.subtest.ok = 0;

    for (const ut_result_t *child = r->_.subtest.head; child != NULL; child = child->next) {
        r->_.subtest.total += 1;
        r->_.subtest.ok += ut_result_ok(child) ? 1 : 0;
    }
}

static inline double ut_result_subtest_duration(const ut_result_t *r) {
    assert(r != NULL);
    assert(r->type == ut_result_type_subtest);

    return 1e3 * (r->_.subtest.end_time - r->_.subtest.start_time) / CLOCKS_PER_SEC;
}

static inline int ut_result_ok(const ut_result_t *r) {
    assert(r != NULL);

    switch (r->type) {
    case ut_result_type_subtest:
        return r->_.subtest.ok == r->_.subtest.total;
    case ut_result_type_assert:
        return r->_.assert.ok;
    default:
        return 0;
    }
}

#define UT_ANSI_RED "\x1b[31m"
#define UT_ANSI_GREEN "\x1b[32m"
#define UT_ANSI_YELLOW "\x1b[33m"
#define UT_ANSI_BLUE "\x1b[34m"
#define UT_ANSI_RESET "\x1b[m"

#define UT_COLOR_INFO(s) UT_ANSI_BLUE s UT_ANSI_RESET
#define UT_COLOR_ERROR(s) UT_ANSI_RED s UT_ANSI_RESET
#define UT_COLOR_WARN(s) UT_ANSI_YELLOW s UT_ANSI_RESET
#define UT_COLOR_SUCCESS(s) UT_ANSI_GREEN s UT_ANSI_RESET

static inline void ut_result_dump_i(const ut_result_t *r, FILE *out, unsigned int depth) {
    assert(r != NULL);

    const bool ok = ut_result_ok(r);

    for (unsigned int i = 0; i < depth; i++) {
        fprintf(out, "  ");
    }

    switch (r->type) {
    case ut_result_type_subtest: {
        if (ok) {
            fprintf(out, UT_COLOR_SUCCESS("[ PASS ] %s"), r->message);
            fprintf(out, " ");
            fprintf(out, UT_COLOR_INFO("[%d/%d]"), r->_.subtest.ok, r->_.subtest.total);
            fprintf(out, " - ");
            fprintf(out, UT_COLOR_INFO("%.1f ms"), ut_result_subtest_duration(r));
            fprintf(out, "\n");

#if UT_VERBOSE
            for (const ut_result_t *child = r->_.subtest.head; child != NULL; child = child->next) {
                ut_result_dump_i(child, out, depth + 1);
            }
#endif
        } else {
            fprintf(out, UT_COLOR_WARN("[FAILED] %s"), r->message);
            fprintf(out, " ");
            fprintf(out, UT_COLOR_INFO("[%d/%d]"), r->_.subtest.ok, r->_.subtest.total);
            fprintf(out, " - ");
            fprintf(out, UT_COLOR_INFO("%.1f ms"), ut_result_subtest_duration(r));
            fprintf(out, " at %s:%llu", r->file, r->line);
            fprintf(out, "\n");

            for (const ut_result_t *child = r->_.subtest.head; child != NULL; child = child->next) {
                ut_result_dump_i(child, out, depth + 1);
            }
        }
        break;
    }
    case ut_result_type_assert: {
        if (ok) {
            fprintf(out, UT_COLOR_SUCCESS("[ PASS ] %s"), r->_.assert.expr);
            if (r->message != NULL) {
                fprintf(out, UT_COLOR_SUCCESS(": %s"), r->message);
            }
        } else {
            fprintf(out, UT_COLOR_ERROR("[FAILED] %s"), r->_.assert.expr);
            if (r->message != NULL) {
                fprintf(out, UT_COLOR_ERROR(": %s"), r->message);
            }
            fprintf(out, " at %s:%llu", r->file, r->line);
        }
        fprintf(out, "\n");
        break;
    }
    default:
        break;
    }
}

static inline void ut_result_dump(const ut_result_t *r, FILE *out) {
    assert(r != NULL);
    assert(r->type == ut_result_type_subtest);

    for (const ut_result_t *child = r->_.subtest.head; child != NULL; child = child->next) {
        ut_result_dump_i(child, out, 0);
    }

    if (ut_result_ok(r)) {
        fprintf(out, UT_COLOR_SUCCESS("[%d/%d]"), r->_.subtest.ok, r->_.subtest.total);
    } else {
        fprintf(out, UT_COLOR_ERROR("[%d/%d]"), r->_.subtest.ok, r->_.subtest.total);
    }

    fprintf(out, " ");
    fprintf(out, UT_COLOR_INFO("total %.1f ms"), ut_result_subtest_duration(r));
    fprintf(out, "\n");
}

static inline void ut_tester_init(ut_tester_t *t, ut_result_t *subtest) {
    assert(t != NULL);
    assert(subtest != NULL);
    assert(subtest->type == ut_result_type_subtest);

    t->current = subtest;
    t->tail = &t->current->_.subtest.head;
}

static inline ut_tester_t *ut_tester_subtest_start(ut_tester_t *t, ut_tester_t *subtester, const char *file, unsigned long long line, const char *format, ...) {
    assert(t != NULL);
    assert(subtester != NULL);

    va_list args;
    va_start(args, format);
    ut_result_t *subtest = ut_result_new_subtest_v(t->current, file, line, format, args);
    va_end(args);

    ut_tester_push_result(t, subtest);

    ut_tester_init(subtester, subtest);

    ut_result_subtest_start(subtest);

    return subtester;
}

static inline void ut_tester_subtest_finish(ut_tester_t *subtester) {
    assert(subtester != NULL);

    ut_result_subtest_finish(subtester->current);
}

static inline void ut_tester_push_result(ut_tester_t *t, ut_result_t *result) {
    assert(t != NULL);
    assert(result != NULL);

    result->next = NULL;
    *t->tail = result;
    t->tail = &result->next;
}

static inline void ut_tester_expect(ut_tester_t *t, const char *file, unsigned long long line, const char *expr, bool ok, const char *format, ...) {
    assert(t != NULL);

    va_list args;
    va_start(args, format);
    ut_tester_push_result(t, ut_result_new_assert_v(t->current, file, line, expr, ok, format, args));
    va_end(args);
}

static inline bool ut_tester_assert(ut_tester_t *t, const char *file, unsigned long long line, const char *expr, bool ok, const char *format, ...) {
    assert(t != NULL);

    va_list args;
    va_start(args, format);
    ut_tester_push_result(t, ut_result_new_assert_v(t->current, file, line, expr, ok, format, args));
    va_end(args);

    if (!ok) {
        for (ut_result_t *p = t->current; p != NULL; p = p->parent) {
            ut_result_subtest_finish(p);
        }
    }

    return ok;
}

static inline void ut_tester_fail(ut_tester_t *t, const char *file, unsigned long long line, const char *format, ...) {
    assert(t != NULL);

    va_list args;
    va_start(args, format);
    ut_tester_push_result(t, ut_result_new_assert_v(t->current, file, line, "fail", 0, format, args));
    va_end(args);

    for (ut_result_t *p = t->current; p != NULL; p = p->parent) {
        ut_result_subtest_finish(p);
    }
}

#ifdef UT_MAIN

ut_test_list_t ut_test_list;

static inline void ut_run_test(ut_tester_t *t, const ut_test_def_t *test) {
    assert(test != NULL);

    ut_result_t *subtest = ut_result_new_subtest(t->current, test->file, test->line, "%s", test->testname);
    ut_tester_push_result(t, subtest);

    ut_tester_t u;
    ut_tester_init(&u, subtest);

    if (ut_test_list.setup != NULL) {
        ut_test_list.setup();
    }

    ut_result_subtest_start(subtest);
    test->run(&u);
    ut_result_subtest_finish(subtest);

    if (ut_test_list.teardown != NULL) {
        ut_test_list.teardown();
    }
}

static inline int ut_run_all(const char *file, unsigned long long line) {
    ut_result_t *root = ut_result_new_subtest(NULL, file, line, NULL);
    ut_tester_t t;
    ut_tester_init(&t, root);

    ut_result_subtest_start(root);

    for (const ut_test_def_t *test = ut_test_list.head; test != NULL; test = test->next) {
        ut_run_test(&t, test);
    }

    ut_result_subtest_finish(root);

    ut_result_dump(root, stderr);

    const bool ok = ut_result_ok(root);

    ut_result_delete(root);

    return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}

#endif // UT_MAIN

#endif // INCLUDE_UT_H
