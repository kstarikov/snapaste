### Snapaste
  
A snapping pastebin implementation. Whatever text data posted to it can only be accessed once.


Works through HTTPS only.

#### Requirements

Erlang R16-R18.


SSH certificate and key for your domain.

#### Installation

```sh
git clone https://github.com/kstarikov/snapaste.git
cd snapaste
make
```

#### Running

Edit the `ebin/snapaste.app` and set your host, port and paths to your certificate files. Then run

```sh
make run
```

#### Example usage

*Assuming that Snapaste is running at localhost:8443*

Post data:

```sh
echo "USSR is going to attack USA" | curl -XPOST -d @- https://localhost:8443
```

Get a response:

```sh
201 Created
https://localhost:8443/cnjAOLEMuCft7UNUatzJPye4V6ToPu2zRWPL996QvGOmfsDqDLKmEwx25DZfZKtR
```

Access the URL:

```sh
curl -k https://localhost:8443/cnjAOLEMuCft7UNUatzJPye4V6ToPu2zRWPL996QvGOmfsDqDLKmEwx25DZfZKtR
USSR is going to attack USA
```


Access the URL again:

```sh
curl -k https://localhost:8443/cnjAOLEMuCft7UNUatzJPye4V6ToPu2zRWPL996QvGOmfsDqDLKmEwx25DZfZKtR
404 Not Found
```
