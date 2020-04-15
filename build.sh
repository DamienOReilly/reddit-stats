#!/usr/bin/env bash

# Poor man's web packager

mkdir -p dist
rm dist/*

mkdir -p staging
rm staging/*

cp src/assets/* dist/

elm make src/Main.elm --optimize --output=staging/elm.js
terser elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | terser --mangle --output=staging/elm.min.js
ELM_MD5=$(openssl md5 -r staging/elm.min.js | awk '{print $1}')
ELM_STAGING=elm-min-$ELM_MD5.js
cp staging/elm.min.js dist/$ELM_STAGING

terser src/chart.js | terser --mangle --output=staging/chart.min.js
CHART_MD5=$(openssl md5 -r staging/chart.min.js  | awk '{print $1}')
CHART_STAGING=chart-min-$CHART_MD5.js
cp staging/chart.min.js  dist/$CHART_STAGING

uglifycss src/reddit-stats.css --output staging/reddit-stats.min.css
REDDIT_STATS_MD5=$(openssl md5 -r staging/reddit-stats.min.css| awk '{print $1}')
REDDIT_STATS_STAGING=reddit-stats-$REDDIT_STATS_MD5.css
cp staging/reddit-stats.min.css  dist/$REDDIT_STATS_STAGING

cp src/index.html dist/

gsed -e "s|{{{REDDIT-STATS-CSS}}}|${REDDIT_STATS_STAGING}|g" \
    -e "s|{{{CHART-JS}}}|${CHART_STAGING}|g" \
    -e "s|{{{ELM-JS}}}|${ELM_STAGING}|g" \
    -i dist/index.html
