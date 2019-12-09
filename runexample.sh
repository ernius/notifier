#!/bin/sh

stack run &

sleep 2

gnome-terminal -- /bin/sh -c 'wscat -c http://localhost:8080/subscribe_to_events -x "{ \"access_id\" : \"client1\", \"timestamp\" : 10 }" -w 240; sleep 10'

gnome-terminal -- /bin/sh -c 'wscat -c http://localhost:8080/subscribe_to_events -x "{ \"access_id\" : \"client2\", \"timestamp\" : 10 }" -w 240; sleep 10'

gnome-terminal -- /bin/sh -c 'wscat -c http://localhost:8080/subscribe_to_events -x "{ \"access_id\" : \"client3\", \"timestamp\" : 10 }" -w 240; sleep 10'

sleep 2

curl -H "Content-Type: application/json" -X POST -d '[{"accessid":"client1", "created_at": 30, "payload" : "client1 message"},{"accessid":"client2", "created_at": 20, "payload" : "client2 message"}]' localhost:8080/push_events

sleep 2

curl -H "Content-Type: application/json" -X POST -d '[{"accessid":"client3", "created_at": 30, "payload" : "client1 message"}]' localhost:8080/push_events

pkill notifier-exe


