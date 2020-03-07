FROM archlinux/base:latest

# docker build . -t registry.gitlab.com/tseenshe/haskell-tng.el:26.3
# docker login --username=tseenshe registry.gitlab.com
# docker push registry.gitlab.com/tseenshe/haskell-tng.el:26.3

RUN pacman -Sy &&\
    yes | pacman -Su emacs-nox git python &&\
    yes | pacman -Scc

ENV PATH "/root/.cask/bin:$PATH"
RUN curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
