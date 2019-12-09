# Notifier

Websocket and HTTP server where authenticated users can subscribe to changes.

# API

## Called by app-clients through websocket:

### subscribe_to_events

It opens a websocket connection, accepting a first message in JSON format { "access\_id" : "client1", "timestamp" : 200 }. If succeed, it will keep  open a websocket connection to the client to keep it update on future events (in JSON format) and those that already happened since last_seen. At any moment the notifier can close the connection, in that case it could be becuase it is too busy (too many open connection) or because the user got dropped out. The events themselves are exactly the same json the client would get after the related Get/Post call (including block-querier).

# Example

> wscat -c http://localhost:8080/subscribe_to_events
> connected (press CTRL+C to quit)
> { "access_id" : "client1", "timestamp" : 2000 }
> 

## Called by connectors through HTTP:

### POST push_events

It take a list of events (in JSON format in the request body) to be pushed and it will always return true. Notice that the notifier might actually ignore those events if there's no app-client subscribed interested on those, but this would be transparent to the connector, who will not care whether their notifications are broadcasted or ignored.

# Example

> curl -H "Content-Type: application/json" -X POST -d '[{"accessid":"client1", "created_at": 10, "payload" : "client1 message"},{"accessid":"client2", "created_at": 20, "payload" : "client2 message"}]' localhost:8080/push_events


###  POST drop_user

It force the connection to that user (in JSON format in the request body { "drop\_access\_id" : "client1" }) to be dropped. Notice that even if this call is not made, the notifier could nevertheless drop subscriptions if they get too many or they don't respond to the heartbeat.

# Example

> curl -H "Content-Type: application/json" -X POST -d '{"drop_access_id":"client1"}' localhost:8080/drop_user

# JSON Event formats

{ "accessid" : «access_id», 
  "created\_at" : «timestampt», 
  "payload" : «what the client would receive, that should look the same as it would had get it from an http request»
}

