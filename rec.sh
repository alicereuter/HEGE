#s=(stack ghci)
#asciinema rec -c s stack.cast
mv stack.cast demo.json
docker run --rm -v $PWD:/data asciinema/asciicast2gif -s 2 -t solarized-dark demo.json demo.gif
