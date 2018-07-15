#!/bin/bash
parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$parent_path/src/main/resources/allure/bin/"
bash allure serve "$parent_path/allure-results"
exit
