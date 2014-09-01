FROM nicferrier/elnode-and-nodejs
MAINTAINER nic@ferrier.me.uk
RUN mkdir /home/emacs/emacs-bites
USER root
ADD assets /home/emacs/emacs-bites/assets
ADD creole-source /home/emacs/emacs-bites/creole-source
ADD indexes /home/emacs/emacs-bites/indexes
ADD source-assets /home/emacs/emacs-bites/source-assets
ADD webapp /home/emacs/emacs-bites/webapp
RUN chown -R emacs.emacs /home/emacs/emacs-bites
USER emacs
WORKDIR /home/emacs/emacs-bites
#RUN npm install .
EXPOSE 8006
ENV ETAG 20140816213427254185764
CMD /usr/local/emacs/bin/emacs -daemon -l webapp/bites.el ; tail -f /dev/null
