#include <stdio.h>
#include <stdlib.h>
#include "GeneratedTypes.h"

/* load image into memory */
bool vmhLoadImage(const char *filename, void *memory, uint64_t memsize)
{
    /* assume file is in VMH format */
    char *line = NULL;
    int read;
    size_t len;

    /* deal in chunks of 64 bits */
    uint64_t* M = (uint64_t*)(memory);
    memsize >>= 3;

    FILE *file = fopen(filename, "r");

    if (file == NULL)
    {
        fprintf(stderr, "vmh-utils: could not open VMH file %s.\n",
                filename);
        return false;
    }

    /* current address pointer */
    uint64_t addr = 0;

    while ((read = getline(&line, &len, file)) != -1)
    {
        if (read != 0)
        {
            /* is it a new address segment? */
            if (line[0] == '@')
            {
                /* recover the rest of the address */
                addr = strtoull(&line[1], NULL, 16);

                /* address in VMH file is already double word
                 * aligned, so no shifting required */
            }
            else
            {
                /* read in the 64-bit aligned data element
                   and store it in memory at the current
                   address
                   
                   NOTE: at the moment, since we only allow
                   64-bit aligned loads/stores, we do not
                   care about endianness.
                          *** TODO: ENDIANNESS ***
                   */
                uint64_t data = strtoull(line, NULL, 16);
                M[addr] = data;// (data << 32) | (data >> 32);

                /* increment address pointer - since our
                   array is UINT64-aligned, we have to
                   increment the address by 1 to get to
                   the next word */
                addr = addr + 1;
            }

            /* make sure we don't overflow! */
            if (addr >= memsize)
            {
                fprintf(stderr, "memory overflow: image is too large.\n");
                fprintf(stdout, "memory overflow: image is too large.\n");
                fclose(file);
                return false;
            }
        }
        free(line);
        line = NULL;
    }

    fclose(file);

    return true;
}
