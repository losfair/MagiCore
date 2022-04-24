ENTRY(_start)

SECTIONS
{
   . = 0x08000000;
   .text : { *(.text.entry) *(.text) *(.text.*) }
   .data : { *(.data) *(.data.*) *(.rodata) *(.rodata.*) *(.sdata) *(.sdata.*) }
   .bss : { *(.bss) }
}
