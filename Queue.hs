module Queue where

data Queue a = Queue [a] [a]

queueHead (Queue [] [])       = error "empty queue!"
queueHead (Queue [] enq)      = (x, Queue deq []) where (x:deq) = reverse enq
queueHead (Queue (x:deq) enq) = (x, Queue deq enq)

queue (Queue deq enq) xs = Queue deq (xs ++ enq)

queueFromList enq = Queue enq []
