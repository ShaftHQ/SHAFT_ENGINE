#! /usr/bin/env bash

unset JAVA_HOME

mkdir -p ./${INPUT_GH_PAGES}
mkdir -p ./${INPUT_ALLURE_HISTORY}
cp -r ./${INPUT_GH_PAGES}/. ./${INPUT_ALLURE_HISTORY}

REPOSITORY_OWNER_SLASH_NAME=${INPUT_GITHUB_REPO}
REPOSITORY_NAME=${REPOSITORY_OWNER_SLASH_NAME##*/}
GITHUB_PAGES_WEBSITE_URL="https://${INPUT_GITHUB_REPO_OWNER}.github.io/${REPOSITORY_NAME}"
#echo "Github pages url $GITHUB_PAGES_WEBSITE_URL"

if [[ ${INPUT_SUBFOLDER} != '' ]]; then
    INPUT_ALLURE_HISTORY="${INPUT_ALLURE_HISTORY}/${INPUT_SUBFOLDER}"
    INPUT_GH_PAGES="${INPUT_GH_PAGES}/${INPUT_SUBFOLDER}"
    echo "NEW allure history folder ${INPUT_ALLURE_HISTORY}"
    mkdir -p ./${INPUT_ALLURE_HISTORY}
    GITHUB_PAGES_WEBSITE_URL="${GITHUB_PAGES_WEBSITE_URL}/${INPUT_SUBFOLDER}"
    echo "NEW github pages url ${GITHUB_PAGES_WEBSITE_URL}"
fi

COUNT=$( ( ls ./${INPUT_ALLURE_HISTORY} | wc -l ) )
echo "count folders in allure-history: ${COUNT}"
echo "keep reports count ${INPUT_KEEP_REPORTS}"
INPUT_KEEP_REPORTS=$((INPUT_KEEP_REPORTS+1))
echo "if ${COUNT} > ${INPUT_KEEP_REPORTS}"
if (( COUNT > INPUT_KEEP_REPORTS )); then
  cd ./${INPUT_ALLURE_HISTORY}
  echo "remove index.html last-history"
  rm index.html last-history -rv
  echo "remove old reports"
  ls | sort -n | head -n -$((${INPUT_KEEP_REPORTS}-2)) | xargs rm -rv;
  cd ${GITHUB_WORKSPACE}
fi

#echo "index.html"
echo "<!DOCTYPE html><meta charset=\"utf-8\"><meta http-equiv=\"refresh\" content=\"0; URL=${GITHUB_PAGES_WEBSITE_URL}/${INPUT_GITHUB_RUN_NUM}/\">" > ./${INPUT_ALLURE_HISTORY}/index.html # path
echo "<meta http-equiv=\"Pragma\" content=\"no-cache\"><meta http-equiv=\"Expires\" content=\"0\">" >> ./${INPUT_ALLURE_HISTORY}/index.html
#cat ./${INPUT_ALLURE_HISTORY}/index.html

#echo "executor.json"
echo '{"name":"GitHub Actions","type":"github","reportName":"Allure Report with history",' > executor.json
echo "\"url\":\"${GITHUB_PAGES_WEBSITE_URL}\"," >> executor.json # ???
echo "\"reportUrl\":\"${GITHUB_PAGES_WEBSITE_URL}/${INPUT_GITHUB_RUN_NUM}/\"," >> executor.json
echo "\"buildUrl\":\"https://github.com/${INPUT_GITHUB_REPO}/actions/runs/${INPUT_GITHUB_RUN_ID}\"," >> executor.json
echo "\"buildName\":\"GitHub Actions Run #${INPUT_GITHUB_RUN_ID}\",\"buildOrder\":\"${INPUT_GITHUB_RUN_NUM}\"}" >> executor.json
#cat executor.json
mv ./executor.json ./${INPUT_ALLURE_RESULTS}

#environment.properties
echo "URL=${GITHUB_PAGES_WEBSITE_URL}" > environment.properties
mv ./environment.properties ./${INPUT_ALLURE_RESULTS}

echo "keep allure history from ${INPUT_GH_PAGES}/last-history to ${INPUT_ALLURE_RESULTS}/history"
cp -r ./${INPUT_GH_PAGES}/last-history/. ./${INPUT_ALLURE_RESULTS}/history

echo "generating report from ${INPUT_ALLURE_RESULTS} to ${INPUT_ALLURE_REPORT} ..."
#ls -l ${INPUT_ALLURE_RESULTS}
allure generate --clean ${INPUT_ALLURE_RESULTS} -o ${INPUT_ALLURE_REPORT}
#echo "listing report directory ..."
#ls -l ${INPUT_ALLURE_REPORT}

echo "copy allure-report to ${INPUT_ALLURE_HISTORY}/${INPUT_GITHUB_RUN_NUM}"
cp -r ./${INPUT_ALLURE_REPORT}/. ./${INPUT_ALLURE_HISTORY}/${INPUT_GITHUB_RUN_NUM}
echo "copy allure-report history to /${INPUT_ALLURE_HISTORY}/last-history"
cp -r ./${INPUT_ALLURE_REPORT}/history/. ./${INPUT_ALLURE_HISTORY}/last-history