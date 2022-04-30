FROM openjdk:8-jdk-alpine
RUN \
  echo "**** install build packages ****" && \
  apk add --no-cache \
    dos2unix \
    curl \
    wget \
    unzip \
    bash && \
  curl -s https://api.github.com/repos/Szyler/AscensionChat/releases/latest \
    | grep "browser_download_url.*zip" \
    | cut -d '"' -f 4 \
    | wget -qi - && \
  unzip Ascensionchat-*.zip && \
  rm Ascensionchat-*.zip && \
  mv wowchat/* ./ && \
  rm -R wowchat/ && \
  dos2unix run.sh && \
  apk del \
    dos2unix \
    curl \
    wget \
    unzip

ENTRYPOINT ["bash","run.sh"]
