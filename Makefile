install:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests --enable-benchmarks . remote-json-server/ remote-json-client/ remote-json-test/
	cabal configure --enable-tests --enable-benchmarks -v2
	-cabal sandbox hc-pkg unregister remote-json-client
	-cabal sandbox hc-pkg unregister remote-json-server
	cabal install

	(cd remote-json-client ; cabal sandbox init --sandbox=../.cabal-sandbox )
	(cd remote-json-client ; cabal install --only-dependencies --enable-tests --enable-benchmarks )
	(cd remote-json-client ; cabal configure --enable-tests --enable-benchmarks -v2 )
	(cd remote-json-client ; cabal install )

	(cd remote-json-server ; cabal sandbox init --sandbox=../.cabal-sandbox )
	(cd remote-json-server ; cabal install --only-dependencies --enable-tests --enable-benchmarks )
	(cd remote-json-server ; cabal configure --enable-tests --enable-benchmarks -v2 )
	(cd remote-json-server ; cabal install )

test::
	make install
	cabal test
	(cd remote-json-test ; cabal install --only-dependencies --enable-tests --enable-benchmarks )
	(cd remote-json-test ; cabal sandbox init --sandbox=../.cabal-sandbox )
	(cd remote-json-test ; cabal configure --enable-tests --enable-benchmarks -v2 )
	(cd remote-json-test ; cabal test )


