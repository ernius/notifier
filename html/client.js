function create(n) {
  return () => {
    connections[n-1] = new WebSocket("ws://localhost:8080/subscribe_to_events");

    document.querySelector("#disconnect"+n).addEventListener("click", () => {  connections[n-1].close() });
    document.querySelector("#connect"+n).disabled = true;
    
    connections[n-1].onopen = (event) => {
      console.log("WebSocket client "+n+" is open now.");
      // send connection JSON
      connections[n-1].send(JSON.stringify({ access_id : "client" + n ,timestamp : 2000 }));
      document.querySelector("#disconnect"+n).disabled = false;          
    };

    connections[n-1].onclose = (event) => {
      console.log("WebSocket client "+n+" is closed now.");
      document.querySelector("#disconnect"+n).disabled = true;
      document.querySelector("#connect"+n).disabled = false;      
    };
    
    connections[n-1].onerror = (event) => {
      console.error("WebSocket client "+n+" error observed:", event);
      document.querySelector("#disconnect"+n).disabled = true;
      document.querySelector("#connect"+n).disabled = false;      
    };

    connections[n-1].onmessage = (event) => {
      // append received message from the server to the DOM element 
      const chat = document.querySelector("#events"+n);
      var objs = JSON.parse(event.data);
      objs.forEach(obj => chat.innerHTML += obj.payload + '<br>')
    }
  }
}

var connections = new Array(columns);

var width = 100/columns;

for(i = 1;i<=columns;i++) {
  document.getElementById("parentId").innerHTML += `
		 <div id="clientdiv`+i+`" style="float:left;width:`+width+`%">
		   <h1>Client `+i+`</h1>
		   <button id="connect`+i+`">Connect</button><br>
		   <button id="disconnect`+i+`">Disconnect</button><br>
		   <h2>Events</h2>    
		   <div id="events`+i+`"></div>
		 </div>`;
}

for(i = 1;i<=columns;i++) {		 
  document.querySelector("#connect"+i).addEventListener("click", create(i));		 
  document.querySelector("#disconnect"+i).disabled = true;
}

