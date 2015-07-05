if [ "$TRAVIS_REPO_SLUG" == "hawkw/scala-common" ] &&
   [ "$TRAVIS_PULL_REQUEST" == "false" ] &&
   [ ! -z "$TRAVIS_TAG" ]; then


   	echo -e "Publishing ScalaDoc...\n"

   	cp -R target/scala-2.11/api $HOME/api

   	cd $HOME
   	git config --global user.email "travis@travis-ci.org"
   	git config --global user.name "travis-ci"
   	git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/hawkw/scala-common gh-pages > /dev/null

   	cd gh-pages
   	git rm -rf ./api
   	cp -Rf $HOME/api .
   	git add -f .
   	git commit -m "Lastest ScalaDoc for $TRAVIS_TAG on successful travis build $TRAVIS_BUILD_NUMBER auto-pushed to gh-pages"
   	git push -fq origin gh-pages > /dev/null

   	echo -e "Published ScalaDoc to gh-pages.\n"

fi
