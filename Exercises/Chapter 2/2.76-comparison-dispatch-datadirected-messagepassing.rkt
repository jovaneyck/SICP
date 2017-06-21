#lang racket

#|
* impact of adding a new type
    * generic operations with explicit dispatch: every generic operation must be updated with a dispatch for the new type
    * data-directed style: new type has to install all operations for this type, no existing code needs to be updated.
¨   * message-passing: no existing code has to be updated.
* impact of adding a new operation
    * generic operations with explicit dispatch: all types must implement their version + new operation must dispatch to all the existing types.
    * data-directed style: all existing types must install their implementation for this operation in the table
    * mesage-passing: all existing types must accept this new message.
* which system most appropriate when new types get added often?
    Data-directed or message-passing both are open-closed with regards to adding new types.
    Message-passing has added benefit that there's no global lookup table necessary, just the single generic dispatch operation.
* which system most appropriate when new operations get added often?
    Also data-directed or message passing? They have added benefit that you don't need explicit knowledge about all implementations at once.
|#