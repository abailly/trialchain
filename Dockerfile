FROM alpine:3.8

ENV LANG C.UTF-8

RUN apk add --no-cache \
      ca-certificates \
      curl \
      gmp-dev \
      ncurses5-libs \
      libc6-compat \
      zlib-dev \
      libbsd-dev &&\
      ln -s /lib /lib64 && \
      ln -s /usr/lib /usr/lib64 && \
      ln -s -f /usr/lib/libncurses.so.5.9 /usr/lib/libtinfo.so.5 && \
      mkdir /work && \
      chmod 777 /work

WORKDIR /work

COPY bin/training /bin

ENTRYPOINT ["/bin/training" ]

STOPSIGNAL SIGTERM
