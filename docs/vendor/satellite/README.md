satellite.js 7.1.0 (MIT, see LICENSE.md), bundled to one browser file with only
the functions the site uses (no WebAssembly part):

    npm install satellite.js@7.1.0 esbuild
    npx esbuild bundle-entry.js --bundle --minify --format=iife --global-name=satellite --outfile=satellite.min.js

(Run it in the folder where you ran npm install: bundle-entry.js imports
node_modules/satellite.js/dist/*.js directly, because the package's entry
point pulls in the WebAssembly part and its exports block deep imports.)
