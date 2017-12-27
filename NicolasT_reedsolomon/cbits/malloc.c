#include <stdlib.h>
#include <errno.h>

/* A `malloc` which behaves like GNU libc `malloc`:
 * - Return a valid pointer for size 0
 * - Set `errno` to `ENOMEM` on `NULL` return (according to POSIX)
 */
void * rpl_malloc(const size_t size) {
        void * result = NULL;

        result = malloc(size == 0 ? 1 : size);

        if(result == NULL) {
                errno = ENOMEM;
        }

        return result;
}
