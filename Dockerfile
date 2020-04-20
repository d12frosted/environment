FROM d12frosted/environment:archlinux
MAINTAINER Boris Buliga <boris@d12frosted.io>
WORKDIR /opt/eru
COPY ./eru-new.sh /opt/eru/eru-new.sh
RUN ./eru-new.sh
