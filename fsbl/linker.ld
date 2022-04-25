OUTPUT_ARCH(riscv)

ENTRY(_start)

SECTIONS
{
    . = 0x00010000;

    .text : {
        *(.text.entry)
        *(.text .text.*)
    }

    .rodata : {
        *(.rodata .rodata.*)
    }

    .data : {
        *(.data .data.*)
        *(.sdata .sdata.*)
    }

    .bss : {
        *(.sbss .bss .bss.*)
    }

    /DISCARD/ : { *(.eh_frame) }
}