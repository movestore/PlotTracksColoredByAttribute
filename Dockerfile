########################################################################################################################
# MoveApps R-SHINY SDK
########################################################################################################################

FROM rocker/geospatial:4.4.2

LABEL maintainer = "couchbits GmbH <us@couchbits.com>"

# Security Aspects
# group `staff` b/c of:
# When running rocker with a non-root user the docker user is still able to install packages.
# The user docker is member of the group staff and could write to /usr/local/lib/R/site-library.
# https://github.com/rocker-org/rocker/wiki/managing-users-in-docker
RUN useradd --create-home --shell /bin/bash moveapps --groups staff

# Install Google Chrome for webshot2/chromote (PNG export of leaflet maps).
# Note: Ubuntu's `chromium` package is a snap stub that does not work in
# containers; we install Chrome from Google's APT repo instead.
# No CHROMOTE_* env needed: chromote auto-detects /usr/bin/google-chrome and
# already passes --no-sandbox/--disable-dev-shm-usage via default_chrome_args().
RUN apt-get update && apt-get install -y --no-install-recommends \
        wget gnupg ca-certificates \
    && wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub \
        | gpg --dearmor -o /usr/share/keyrings/google-linux.gpg \
    && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-linux.gpg] https://dl.google.com/linux/chrome/deb/ stable main" \
        > /etc/apt/sources.list.d/google-chrome.list \
    && apt-get update && apt-get install -y --no-install-recommends \
        google-chrome-stable \
    && rm -rf /var/lib/apt/lists/*

USER moveapps:staff

WORKDIR /home/moveapps/co-pilot-r

# copy the SDK
COPY --chown=moveapps:staff src/ ./src/
COPY --chown=moveapps:staff data/ ./data/
COPY --chown=moveapps:staff www/ ./www/
COPY --chown=moveapps:staff sdk.R ShinyModule.R .env start-process.sh ./

# restore the current snapshot via renv
COPY --chown=moveapps:staff renv.lock .Rprofile ./
COPY --chown=moveapps:staff renv/activate.R renv/settings.dcf ./renv/
RUN R -e 'renv::restore()'

# shiny port
EXPOSE 3838

ENTRYPOINT ["/bin/bash"]