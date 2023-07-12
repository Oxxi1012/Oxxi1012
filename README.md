
-->
#include "protheus.ch"
//geração do bloco F100 com base em registros contabeis

User Function SPDPIS09
Local aRetF100 := {}
Local cCT2     := ""
Local cCtaSPED := "%" + FormatIn(GetMV("MV_CTASPED")+AllTrim(GetMV("MV_CTASPE2")), "/") + "%"
Local cCtaApl  := "%" + FormatIn(GetMV("MV_CTAAPL"), "/") + "%"
Local cContas  := ""
Local cCtaDesc := ""
Local cCta     := ""
Local cBanco   := ""
Local dData    := SToD("")
Local dDataDe  := SToD("")
Local dDataAte := SToD("")
Local nDeb     := 0
Local nCred    := 0
Local nSaldo   := 0
Local nTxPIS   := 0
Local nTxCOF   := 0
Local nDesc    := 0
Local nI       := 0
Local nTotal   := 0
Local nFator   := 0
Local lConta   := .F.

//cFilAnt  := ParamIXB[1]
dDataDe	 := ParamIXB[2]
dDataAte := ParamIXB[3]

If SubStr(SM0->M0_CGC, 9, 4) == "0001" // matriz
	cCT2  := GetNextAlias()
	BeginSQL Alias cCT2
		COLUMN DATAMOV AS DATE
		SELECT  DATAMOV, CONTA, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP, SUM(SALDO) AS SALDO
		FROM (
		SELECT  CT2_DEBITO AS CONTA, CT2_DATA AS DATAMOV, CT2_VALOR * -1 AS SALDO, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP
		FROM    %table:CT2% CT2, %table:CT1% CT1
		WHERE   CT2_DEBITO = CT1_CONTA
		AND		CT2.%notdel%
		AND		CT1.%notdel%
		AND		CT2_FILIAL = %xfilial:CT2%
		AND		CT1_FILIAL = %xfilial:CT1%
		AND     CT2_DATA BETWEEN %exp:dDataDe% AND %exp:dDataAte%
		AND CT2_GSPED <> '2' AND CT2_MOEDLC = '01'
		AND     CT2_DEBITO IN %exp:cCtaSPED%
		UNION ALL
		SELECT  CT2_CREDIT AS CONTA, CT2_DATA AS DATAMOV, CT2_VALOR AS SALDO, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP
		FROM    %table:CT2% CT2, %table:CT1% CT1
		WHERE   CT2_CREDIT = CT1_CONTA
		AND		CT2.%notdel%
		AND		CT1.%notdel%
		AND		CT2_FILIAL = %xfilial:CT2%
		AND		CT1_FILIAL = %xfilial:CT1%
		AND     CT2_DATA BETWEEN %exp:dDataDe% AND %exp:dDataAte%
		AND CT2_GSPED <> '2' AND CT2_MOEDLC = '01'
		AND     CT2_CREDIT IN %exp:cCtaSPED%) CT2
		GROUP BY DATAMOV, CONTA, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP
		ORDER BY DATAMOV, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP, CONTA
	EndSQL
	While !Eof()
		dData   := DATAMOV
		cCstPIS := CT1_CSTPIS
		cCstCOF := CT1_CSTCOF
		cCodBCC := CT1_CODBCC
		cTNatRe := CT1_TNATRE
		cCNatRe := CT1_CNATRE
		cGBaseP := CT1_GBASEP
		//cContas := ""
		cContas := CONTA
		nSaldo  := 0
		
		//While !Eof() .and. (DToS(DATAMOV) + CT1_CSTPIS + CT1_CSTCOF + CT1_CODBCC + CT1_TNATRE + CT1_CNATRE + CT1_GBASEP == DToS(dData) + cCstPIS + cCstCOF + cCodBCC + cTNatRe + cCNatRe + cGBaseP)
		While !Eof() .and. (DToS(DATAMOV) + CONTA + CT1_CSTPIS + CT1_CSTCOF + CT1_CODBCC + CT1_TNATRE + CT1_CNATRE + CT1_GBASEP == DToS(dData) + cContas + cCstPIS + cCstCOF + cCodBCC + cTNatRe + cCNatRe + cGBaseP)
			//cContas += If(Empty(cContas), "", ", ") + AllTrim(CONTA)
			nSaldo  += SALDO
			
			dbSkip()
		EndDo
		
		cIndOP := ""
		If cCstPIS $ "50/51/52/53/54/55/56/60/61/62/63/64/65/66"
			cIndOP := "0"
		ElseIf cCstPIS $ "01/02/03/05"
			cIndOP := "1"
		ElseIf cCstPIS $ "04/06/07/08/09/49/99"
			cIndOP := "2"
		Else
			cIndOP := "X"
		EndIf
		
		dbSelectArea("CCZ")
		dbSetOrder(1)
		dbSeek(xFilial() + cTNatRe + cCNatRe)
		
		nTxPIS := If(cCstPIS == "01", GetMV("MV_TXPIS"), CCZ->CCZ_ALQPIS)
		nTxCOF := If(cCstCOF == "01", GetMV("MV_TXCOFIN"), CCZ->CCZ_ALQCOF)
		
		Aadd( aRetF100, {'F100',;	// F100 - 01 - REG
		cIndOP				,;	// F100 - 02 - IND_OPER  ( 0 - Entrada, >0 - Saida )
		''					,;	// F100 - 03 - COD_PART (Entrada= SA2->A2_COD, Saida=  SA1->A1_COD)
		''					,;	// F100 - 04 - COD_ITEM
		ConvData(dData)		,;	// F100 - 05 - DT_OPER
		nSaldo				,;	// F100 - 06 - VL_OPER
		cCstPIS				,;	// F100 - 07 - CST_PIS
		Iif(cGBaseP # "1", 0, nSaldo),;	// F100 - 08 - VL_BC_PIS
		Iif(cGBaseP # "1", 0, nTxPIS),;	// F100 - 09 - ALIQ_PIS
		Iif(cGBaseP # "1", 0, Round(nSaldo * nTxPIS / 100, 2)),;	// F100 - 10 - VL_PIS
		cCstCOF				,;	// F100 - 11 - CST_COFINS
		Iif(cGBaseP # "1", 0, nSaldo),;	// F100 - 12 - VL_BC_COFINS
		Iif(cGBaseP # "1", 0, nTxCOF),;	// F100 - 13 - ALIQ_COFINS
		Iif(cGBaseP # "1", 0, Round(nSaldo * nTxCOF / 100, 2)),;	// F100 - 14 - VL_COFINS
		cCodBCC				,;	// F100 - 15 - NAT_BC_CRED
		'0'					,;	// F100 - 16 - IND_ORIG_CRED
		cContas				,;	// F100 - 17 - COD_CTA
		''					,;	// F100 - 18 - COD_CCUS
		Posicione("CT1",1,xFilial("CT1")+cContas,"CT1_DESC01")	,;	// F100 - 19 - DESC_DOC_OPER
		''					,;	// F100 - 20 - LOJA (Entarada = SA2->A2_LOJA, Saida = SA1->A1_LOJA)
		STR(MV_PAR06)		,;	// F100 - 21 - INDICE DE CUMULATIVIDADE( 0 - Cumulativo, 1 - Nao cumultivo )
		CVB->CVB_CODCTB		,;	// 0150 - 02 - COD_PART
		CVB->CVB_NOME		,;	// 0150 - 03 - NOME
		CVB->CVB_PAIS		,; 	// 0150 - 04 - COD_PAIS
		CVB->CVB_CGC		,;	// 0150 - 05 - CNPJ
		CVB->CVB_CPF		,;	// 0150 - 06 - CPF
		''					,; 	// 0150 - 07 - IE
		CVB->CVB_CODMUN		,; 	// 0150 - 08 - COD_MUN
		''					,; 	// 0150 - 09 - SUFRAMA
		''					,; 	// 0150 - 10 - END
		''					,; 	// 0150 - 11 - NUM
		''					,;	// 0150 - 12 - COMPL
		''					,;	// 0150 - 13 - BAIRRO
		ctod("01/01/80")	,;	// 0500 - 02 - DT_ALT
		Posicione("CT1",1,xFilial("CT1")+cContas,"CT1_NTSPED")	,;	// 0500 - 03 - COD_NAT_CC
		'A'					,;	// 0500 - 04 - IND_CTA
		'5'					,;	// 0500 - 05 - NIVEL
		cContas				,;	// 0500 - 06 - COD_CTA
		AllTrim(Posicione("CT1",1,xFilial("CT1")+cContas,"CT1_DESC01"))	,;	// 0500 - 07 - NOME_CTA
		''					,;	// 0500 - 08 - COD_CTA_REF
		''					,;	// 0500 - 09 - CNPJ_EST
		cTNatRe				,;	// Codigo da tabela da Natureza da Receita.
		cCNatRe				,;	// Codigo da Natureza da Receita
		''					,;	// Grupo da Natureza da Receita
		ctod("//")			})	// Dt.Fim Natureza da Receita
		
		dbSelectArea(cCT2)
	EndDo
	dbCloseArea()
	nDesc  := ValDesc(dDataDe, dDataAte, @cCtaDesc)
	nParc  := 0
	nTotal := 0
	aEval(aRetF100, {|aReg| nTotal += aReg[8]})
	For nI := 1 To Len(aRetF100)
		/*
		If aRetF100[nI][08] > 0
			nFator := (nDesc * 100) / nTotal
			nParc  := round((aRetF100[nI][08] * nFator) / 100,2)
			aRetF100[nI][08] -= round(nParc,2)
			aRetF100[nI][10] := aRetF100[nI][08] * aRetF100[nI][09] / 100
			aRetF100[nI][12] -= round(nParc,2)
			aRetF100[nI][14] := aRetF100[nI][12] * aRetF100[nI][13] / 100
			aRetF100[nI][19] += ", " + cCtaDesc
		EndIf
		*/
	Next
	
	cCT2 := GetNextAlias()
	BeginSQL Alias cCT2
		COLUMN DATAMOV AS DATE
		//SELECT  DATAMOV, BANCO, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP, SUM(SALDO) AS SALDO
		SELECT  BANCO, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP, SUM(SALDO) AS SALDO
		FROM (
		SELECT  CT2_DATA AS DATAMOV, CT2_VALOR * -1 AS SALDO, CT1a.CT1_CSTPIS, CT1a.CT1_CSTCOF, CT1a.CT1_CODBCC, CT1a.CT1_TNATRE, CT1a.CT1_CNATRE, CT1a.CT1_GBASEP, CT1b.CT1_INSTBC AS BANCO
		FROM    %table:CT2% CT2, %table:CT1% CT1a, %table:CT1% CT1b
		WHERE   CT2_DEBITO = CT1a.CT1_CONTA
		AND		CT2_CREDIT = CT1b.CT1_CONTA
		AND		CT2.%notdel%
		AND		CT1a.%notdel%
		AND		CT1b.%notdel%
		AND		CT2_FILIAL = %xfilial:CT2%
		AND		CT1a.CT1_FILIAL = %xfilial:CT1%
		AND		CT1b.CT1_FILIAL = %xfilial:CT1%
		AND     CT2_DATA BETWEEN %exp:dDataDe% AND %exp:dDataAte%
		AND CT2_GSPED <> '2' AND CT2_MOEDLC = '01'
		AND     CT2_DEBITO IN %exp:cCtaApl%
		UNION ALL
		SELECT  CT2_DATA AS DATAMOV, CT2_VALOR AS SALDO, CT1a.CT1_CSTPIS, CT1a.CT1_CSTCOF, CT1a.CT1_CODBCC, CT1a.CT1_TNATRE, CT1a.CT1_CNATRE, CT1a.CT1_GBASEP, CT1b.CT1_INSTBC AS BANCO
		FROM    %table:CT2% CT2, %table:CT1% CT1a, %table:CT1% CT1b
		WHERE   CT2_CREDIT = CT1a.CT1_CONTA
		AND		CT2_DEBITO = CT1b.CT1_CONTA
		AND		CT2.%notdel%
		AND		CT1a.%notdel%
		AND		CT1b.%notdel%
		AND		CT2_FILIAL = %xfilial:CT2%
		AND		CT1a.CT1_FILIAL = %xfilial:CT1%
		AND		CT1b.CT1_FILIAL = %xfilial:CT1%
		AND     CT2_DATA BETWEEN %exp:dDataDe% AND %exp:dDataAte%
		AND CT2_GSPED <> '2' AND CT2_MOEDLC = '01'
		AND     CT2_CREDIT IN %exp:cCtaApl%) CT2
		//GROUP BY DATAMOV, BANCO, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP
		//ORDER BY DATAMOV, BANCO, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP
		GROUP BY BANCO, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP
		ORDER BY BANCO, CT1_CSTPIS, CT1_CSTCOF, CT1_CODBCC, CT1_TNATRE, CT1_CNATRE, CT1_GBASEP
	EndSQL
	While !Eof()
		dData   := dDataAte //DATAMOV
		cCstPIS := CT1_CSTPIS
		cCstCOF := CT1_CSTCOF
		cCodBCC := CT1_CODBCC
		cTNatRe := CT1_TNATRE
		cCNatRe := CT1_CNATRE
		cGBaseP := CT1_GBASEP
		cBanco  := BANCO
		nSaldo  := SALDO
		
		cIndOP := ""
		If cCstPIS $ "50/51/52/53/54/55/56/60/61/62/63/64/65/66"
			cIndOP := "0"
		ElseIf cCstPIS $ "01/02/03/05"
			cIndOP := "1"
		ElseIf cCstPIS $ "04/06/07/08/09/49/99"
			cIndOP := "2"
		Else
			cIndOP := "X"
		EndIf
		
		dbSelectArea("CCZ")
		dbSetOrder(1)
		dbSeek(xFilial() + cTNatRe + cCNatRe)
		
		nTxPIS := If(cCstPIS == "01", GetMV("MV_TXPIS"), CCZ->CCZ_ALQPIS)
		nTxCOF := If(cCstCOF == "01", GetMV("MV_TXCOFIN"), CCZ->CCZ_ALQCOF)
		
		Aadd( aRetF100, {'F100',;	// F100 - 01 - REG
		cIndOP				,;	// F100 - 02 - IND_OPER  ( 0 - Entrada, >0 - Saida )
		''					,;	// F100 - 03 - COD_PART (Entrada= SA2->A2_COD, Saida=  SA1->A1_COD)
		''					,;	// F100 - 04 - COD_ITEM
		ConvData(dData)		,;	// F100 - 05 - DT_OPER
		nSaldo				,;	// F100 - 06 - VL_OPER
		cCstPIS				,;	// F100 - 07 - CST_PIS
		Iif(cGBaseP # "1", 0, nSaldo),;	// F100 - 08 - VL_BC_PIS
		Iif(cGBaseP # "1", 0, nTxPIS),;	// F100 - 09 - ALIQ_PIS
		Iif(cGBaseP # "1", 0, Round(nSaldo * nTxPIS / 100, 2)),;	// F100 - 10 - VL_PIS
		cCstCOF				,;	// F100 - 11 - CST_COFINS
		Iif(cGBaseP # "1", 0, nSaldo),;	// F100 - 12 - VL_BC_COFINS
		Iif(cGBaseP # "1", 0, nTxCOF),;	// F100 - 13 - ALIQ_COFINS
		Iif(cGBaseP # "1", 0, Round(nSaldo * nTxCOF / 100, 2)),;	// F100 - 14 - VL_COFINS
		cCodBCC				,;	// F100 - 15 - NAT_BC_CRED
		'0'					,;	// F100 - 16 - IND_ORIG_CRED
		''					,;	// F100 - 17 - COD_CTA
		''					,;	// F100 - 18 - COD_CCUS
		Iif(Empty(cBanco), "SEM VINCULO", "REF. INST. " + cBanco + " " + Tabela("75", cBanco, .F.))	,;	// F100 - 19 - DESC_DOC_OPER
		''					,;	// F100 - 20 - LOJA (Entarada = SA2->A2_LOJA, Saida = SA1->A1_LOJA)
		''					,;	// F100 - 21 - INDICE DE CUMULATIVIDADE( 0 - Cumulativo, 1 - Nao cumultivo )
		CVB->CVB_CODCTB		,;	// 0150 - 02 - COD_PART
		CVB->CVB_NOME		,;	// 0150 - 03 - NOME
		CVB->CVB_PAIS		,; 	// 0150 - 04 - COD_PAIS
		CVB->CVB_CGC		,;	// 0150 - 05 - CNPJ
		CVB->CVB_CPF		,;	// 0150 - 06 - CPF
		''					,; 	// 0150 - 07 - IE
		CVB->CVB_CODMUN		,; 	// 0150 - 08 - COD_MUN
		''					,; 	// 0150 - 09 - SUFRAMA
		''					,; 	// 0150 - 10 - END
		''					,; 	// 0150 - 11 - NUM
		''					,;	// 0150 - 12 - COMPL
		''					,;	// 0150 - 13 - BAIRRO
		ctod("//")			,;	// 0500 - 02 - DT_ALT
		''					,;	// 0500 - 03 - COD_NAT_CC
		''					,;	// 0500 - 04 - IND_CTA
		''					,;	// 0500 - 05 - NIVEL
		''					,;	// 0500 - 06 - COD_CTA
		''					,;	// 0500 - 07 - NOME_CTA
		''					,;	// 0500 - 08 - COD_CTA_REF
		''					,;	// 0500 - 09 - CNPJ_EST
		cTNatRe				,;	//Codigo da tabela da Natureza da Receita.
		cCNatRe				,;	//Codigo da Natureza da Receita
		''					,;	//Grupo da Natureza da Receita
		ctod("//")			})	//Dt.Fim Natureza da Receita
		
		dbSelectArea(cCT2)
		dbSkip()
	EndDo
	dbCloseArea()
EndIf
Return aRetF100

Static Function ConvData(dData)
Local cData := ""

cData += StrZero(Day(dData), 2)
cData += StrZero(Month(dData), 2)
cData += StrZero(Year(dData), 4)
Return cData

Static Function ValDesc(dDataDe, dDataAte, cContas)
Local cCT2  := ""
Local cBkp  := If(Empty(Alias()), "CT2", Alias())
Local cCtaDesc := "%" + FormatIn(GetMV("MV_CTADESC"), "/") + "%"
Local nDesc := 0

cCT2 := GetNextAlias()
BeginSQL Alias cCT2
	SELECT	CONTA, SUM(SALDO) AS SALDO
	FROM (                                                                 	
	SELECT	CT2_DEBITO AS CONTA, SUM(CT2_VALOR) AS SALDO
	FROM	%table:CT2% CT2
	WHERE	CT2.%notdel%
	AND		CT2_FILIAL = %xfilial:CT2%
	AND		CT2_DATA BETWEEN %exp:dDataDe% AND %exp:dDataAte%
	AND CT2_GSPED <> '2' AND CT2_MOEDLC = '01'
	AND		CT2_DEBITO IN %exp:cCtaDesc%
	GROUP BY CT2_DEBITO
	UNION ALL
	SELECT	CT2_CREDIT AS CONTA, SUM(CT2_VALOR * -1) AS SALDO
	FROM	%table:CT2% CT2
	WHERE	CT2.%notdel%
	AND		CT2_FILIAL = %xfilial:CT2%
	AND		CT2_DATA BETWEEN %exp:dDataDe% AND %exp:dDataAte%
	AND CT2_GSPED <> '2' AND CT2_MOEDLC = '01'
	AND		CT2_CREDIT IN %exp:cCtaDesc%
	GROUP BY CT2_CREDIT) CT2
	GROUP BY CONTA
	ORDER BY CONTA
EndSQL
While !Eof()
	cContas += If(Empty(cContas), "", ", ") + AllTrim(CONTA)
	nDesc   += SALDO
	dbSkip()
EndDo
dbCloseArea()
dbSelectArea(cBkp)

