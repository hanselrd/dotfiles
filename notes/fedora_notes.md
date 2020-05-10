# Inspect initramfs
mkdir -p /tmp/i
cd /tmp/i
sudo file /boot/initramfs-$(uname -r).img
<!-- ASCII cpio archive (SVR4 with no CRC -->
sudo cpio -ivdF /boot/initramfs-$(uname -r).img
sudo dd if=/boot/initramfs-$(uname -r).img of=initramfs-$(uname -r).img bs=512 skip=NNNN
sudo zcat initramfs-$(uname -r).img | cpio -ivd

# Update Grub
sudo grub2-mkconfig -o /boot/efi/EFI/fedora/grub.cfg

# Generate keyfile and add as LUKS key
sudo dd if=/dev/random of=/root/cryptbtrfs.keyfile iflag=fullblock bs=512 count=4
sudo chmod 000 /root/cryptbtrfs.keyfile
sudo cryptsetup -v luksAddKey /dev/sda2 /root/cryptbtrfs.keyfile
sudo cryptsetup -v luksDump /dev/sda2

# Generate new initramfs
sudo dracut --install /root/cryptbtrfs.keyfile --force /boot/initramfs-$(uname -r).img
sudo dracut --force /boot/initramfs-$(uname -r).img
