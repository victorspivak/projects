@(implicit r: RequestHeader)

$(function() {

    var WS = window['MozWebSocket'] ? MozWebSocket : WebSocket
    var mySocket = new WS("@routes.Application.startWebSocket.webSocketURL()")

    var sendMessage = function() {
        mySocket.send(JSON.stringify(
            {text: $("#talk").val()}
        ))
        $("#talk").val('')
    }

    var receiveEvent = function(event) {
        var data = JSON.parse(event.data)

        // Handle errors
        if(data.error) {
            mySocket.close()
            $("#onError span").text(data.error)
            $("#onError").show()
            return
        } else {
            $("#talkfromserver").show()
        }

        $("#talkfromserver").val(data.text)
    }

    var handleReturnKey = function(e) {
        if(e.charCode == 13 || e.keyCode == 13) {
            e.preventDefault()
            sendMessage()
        }
    }

    $("#talk").keypress(handleReturnKey)

    mySocket.onmessage = receiveEvent
})
