FROM archlinux/base
RUN pacman -Syu --noconfirm --needed base-devel git pacman-contrib
RUN useradd -m -g users -G wheel -s /bin/bash d12frosted
RUN echo '%wheel ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER d12frosted
WORKDIR /home/d12frosted
CMD ["/usr/bin/bash"]
