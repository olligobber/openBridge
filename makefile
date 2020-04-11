make:
	sass src/style.sass index.css
	spago bundle-app
	spago test
