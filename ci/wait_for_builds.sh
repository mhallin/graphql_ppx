set -e

if [ -z "$TRAVIS_TAG" ]; then
    echo "TRAVIS_TAG variable empty, will not wait for builds"
    exit 0
fi

CURL="curl -ifs"
BASE_URL=https://github.com/mhallin/graphql_ppx/releases/download/$TRAVIS_TAG/graphql_ppx-
START_TIME=$(date "+%s")

## Wait at most 30 minutes for all platform binaries to be ready

for platform in linux-x64 darwin-x64 win-x64 win-x86; do
    echo Checking binary for $platform

    while ! $CURL $BASE_URL$platform.exe > /dev/null; do
        if [ $(( $(date "+%s") - $START_TIME )) -gt 1800 ]; then
            echo Timed out waiting for builds to finish
            exit 1
        fi

        echo Build task for $platform not yet finished, waiting...
        sleep 10
    done
done

echo 'All binaries found'
