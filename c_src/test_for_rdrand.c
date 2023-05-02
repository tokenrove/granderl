#define HAVE_RDRAND
#include "rdrand.h"

int main(void)
{
    rdrand64();
    return 0;
}
