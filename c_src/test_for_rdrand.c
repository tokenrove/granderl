int main(void)
{
    asm("rdrand %%eax" : : : "eax");
    return 0;
}
