FROM mcr.microsoft.com/dotnet/sdk:7.0-jammy

RUN pwsh -Command "Set-PSRepository PSGallery -InstallationPolicy Trusted" \
    && pwsh -Command "Install-Module Psake,Pester,PSScriptAnalyzer -Scope CurrentUser -Force"

COPY . /src

WORKDIR /src

CMD ["pwsh"]
