package org.microg.gms.asterism

import java.time.LocalDate

data class WireRecord(
    val wireReferenceNum: String,
    val wireAmount: Double,
    var wireStatus: String,
    val wireType: String,
    val wireSendDate: LocalDate,
    val wireValueDate: LocalDate,
    var wireLastTxnDate: LocalDate,
    var ledgerBalance: Double = 0.0,
    var holdAmount: Double = 0.0
)

class SagaWireCompensator {
    private var wireRecord: WireRecord? = null

    fun processWire(record: WireRecord) {
        this.wireRecord = record

        if (record.wireStatus == "E0025") {
            handleOfacMatch()
        } else if (record.wireType == "I") {
            handleIncomingWire(record)
        } else if (record.wireType == "O") {
            handleOutgoingWire(record)
        }
    }

    private fun handleOfacMatch() {
        println("OFAC match on originator")
    }

    private fun handleIncomingWire(record: WireRecord) {
        var currentBalance = record.ledgerBalance
        val hold = record.holdAmount

        try {
            currentBalance += record.wireAmount
            currentBalance -= hold

            if (currentBalance < 0) {
                throw ArithmeticException("Insufficient funds for wire transfer")
            }

            record.ledgerBalance = currentBalance
            record.wireLastTxnDate = LocalDate.now()

            println("Wire transfer received successfully")
        } catch (e: ArithmeticException) {
            handleArithmeticOverflow(e)
        }
    }

    private fun handleOutgoingWire(record: WireRecord) {
        var currentBalance = record.ledgerBalance
        val hold = record.holdAmount

        try {
            currentBalance -= record.wireAmount
            currentBalance -= hold

            if (currentBalance < 0) {
                throw ArithmeticException("Insufficient funds for outgoing wire")
            }

            record.ledgerBalance = currentBalance
            record.wireLastTxnDate = LocalDate.now()

            println("Wire transfer sent successfully")
        } catch (e: ArithmeticException) {
            handleArithmeticOverflow(e)
        }
    }

    private fun handleArithmeticOverflow(e: ArithmeticException) {
        println("Arithmetic overflow or validation error: ${e.message}")
    }

    fun processComplete() {
        if (wireRecord?.wireStatus == "CP") {
            updateWireStatus("CP", LocalDate.now())
            println("Wire transfer completed")
        } else {
            handleInvalidWireStatus()
        }
    }

    fun processReverse() {
        if (wireRecord?.wireStatus == "CP") {
            reverseWireTransaction()
            println("Wire transfer reversed")
        } else {
            handleInvalidWireStatus()
        }
    }

    private fun updateWireStatus(status: String, date: LocalDate) {
        wireRecord?.let {
            it.wireStatus = status
            it.wireLastTxnDate = date
            println("Updated wire status to $status")
        }
    }

    private fun reverseWireTransaction() {
        val record = wireRecord ?: return
        var currentBalance = record.ledgerBalance
        val hold = record.holdAmount

        try {
            // Reversão SAGA: Adiciona o valor de volta e ajusta retenções
            currentBalance += record.wireAmount
            currentBalance += hold

            if (currentBalance < 0) {
                throw ArithmeticException("Insufficient funds for wire reversal")
            }

            record.ledgerBalance = currentBalance
            record.wireLastTxnDate = LocalDate.now()

            println("Wire transfer reversal completed")
        } catch (e: ArithmeticException) {
            handleArithmeticOverflow(e)
        }
    }

    private fun handleInvalidWireStatus() {
        println("Only processing wires can be completed or reversed")
    }
}