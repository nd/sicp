(define (serialized-exchange account1 account2)
  (let ((ser1 (account1 'serializer))
        (ser2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
    (if (< id1 id2)
        ((ser1 (ser2 (exchange))) account1 account2)
        ((ser2 (ser1 (exchange))) account1 account2))))