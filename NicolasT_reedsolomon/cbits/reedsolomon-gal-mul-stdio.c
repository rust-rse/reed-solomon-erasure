/* reedsolomon-gal-mul-stdio.c - Galois-field multiplication routine driver
 *
 * Copyright (c) 2016 Nicolas Trangez
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE
 */

#if HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#if defined(_WIN32) && _WIN32
# define USE_WIN32 1
# include <io.h>
# include <fcntl.h>
#else
# define USE_WIN32 0
#endif

#include "reedsolomon.h"

#ifndef PRIsize_t
# if USE_WIN32
#  define PRIsize_t "I"
# else
#  define PRIsize_t "z"
# endif
#endif
#ifndef SCNsize_t
# if USE_WIN32
#  define SCNsize_t "I"
# else
#  define SCNsize_t "z"
# endif
#endif

int read_all(const int fd, uint8_t *vec, size_t count) {
        ssize_t rc = 0;

        while(count > 0) {
                rc = read(fd, vec, count);

                if(rc < 0) {
                        perror("read");
                        return -1;
                }

                count -= rc;
                vec += rc;
        }

        return 0;
}

int write_all(const int fd, const uint8_t *vec, size_t count) {
        ssize_t rc = 0;

        while(count > 0) {
                rc = write(fd, vec, count);

                if(rc < 0) {
                        perror("write");
                        return -1;
                }

                count -= rc;
                vec += rc;
        }

        return 0;
}


int main(int argc, char **argv) {
        int rc = 1;
        size_t size = 0,
               cnt = 0;
        uint8_t *data = NULL,
                *out = NULL,
                low_vector[16] = { 0 },
                high_vector[16] = { 0 };

        if(argc != 2) {
                fprintf(stderr, "Usage: %s SIZE\n", argv[0]);
                rc = 1;
                goto out;
        }

        rc = sscanf(argv[1], "%" SCNsize_t "u", &size);
        if(rc == EOF) {
                perror("sscanf");
                rc = 1;
                goto out;
        }

#if USE_WIN32
        rc = _setmode(STDIN_FILENO, _O_BINARY);
        if(rc == -1) {
                perror("_setmode");
                rc = 1;
                goto out;
        }

        rc = _setmode(STDOUT_FILENO, _O_BINARY);
        if(rc == -1) {
                perror("_setmode");
                rc = 1;
                goto out;
        }
#endif

        rc = read_all(STDIN_FILENO, low_vector, sizeof(low_vector));
        if(rc != 0) {
                rc = 1;
                goto out;
        }

        rc = read_all(STDIN_FILENO, high_vector, sizeof(high_vector));
        if(rc != 0) {
                rc = 1;
                goto out;
        }

        data = malloc(size);
        if(data == NULL) {
                perror("malloc");
                rc = 1;
                goto out;
        }

        rc = read_all(STDIN_FILENO, data, size);
        if(rc != 0) {
                rc = 1;
                goto out;
        }

        out = malloc(size);
        if(out == NULL) {
                perror("malloc");
                rc = 1;
                goto out;
        }

        cnt = reedsolomon_gal_mul(low_vector, high_vector, data, out, size);
        if(cnt != size) {
                fprintf(stderr, "Count mismatch: size=%" PRIsize_t "u, cnt=%" PRIsize_t "u\n", size, cnt);
                rc = 1;
                goto out;
        }

        rc = write_all(STDOUT_FILENO, out, size);
        if(rc != 0) {
                rc = 1;
                goto out;
        }

        rc = 0;

out:
        free(data);
        free(out);

        return rc;
}
