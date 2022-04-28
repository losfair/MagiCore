print("regsave:")
print("addi sp, sp, -248")
for i in range(1, 32):
  print("sd x{}, {}(sp)".format(i, (i - 1) * 8))

print()
print("regrestore:")
for i in range(1, 32):
  prefix = ""
  if i == 10:
    prefix = "# "
  print(prefix + "ld x{}, {}(a0)".format(i, (i - 1) * 8))
print("ld a0, 72(a0)")
print("addi sp, sp, 248")
