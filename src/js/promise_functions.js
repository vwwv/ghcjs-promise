







var counter  = 0;
var booked   = {};

function __js_book_promise(){
    counter = counter + 1;
    return counter;
}

function __js_set_promise(ref){
    return new Promise(
       function(resolve, reject){
           booked[''+ref] = { resolve : resolve
                            , reject  : reject
                            };
       }
    );
}

function __js_do_reject(ref, val){
    booked[''+ref].reject(val);
    delete booked[''+ref]
}


function __js_do_resolve(ref, val){
    booked[''+ref].resolve(val);
    delete booked[''+ref]
}

function __js_await(promise,continuation){
     promise.then(     function(x){                                               
                         continuation({ result : x, ok : true})                             
                      }                                                          
                 );                                                               
                                                                              
     promise['catch']( function(x){                                               
                         continuation({ result : x, ok : false})                            
                        }                                                          
                     );                                                            
}





