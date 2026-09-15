/**************************************************
* Tabelas para interface iBiege - rotacao critica *
*          08:46 30/11/2004 - Francisco           *
***************************************************/
--
--
CREATE SEQUENCE d_IDNBIEGE_SEQ
 INCREMENT BY 1
  START WITH 1
  MINVALUE 0
  NOCYCLE
  NOCACHE
  NOORDER;
--
--
Create table d_idnBIEGE (
  ID      NUMBER (10,0) NOT NULL,
  NR_REF  NUMBER(6,0)   not null, -- Numero de referencia do calculo
  DS_CALC VARCHAR2(50)  null,     -- Descricao opcional do calculo
  NM_USU  VARCHAR2(12)  not null, -- Nome do usuario responsavel
  DT_DAT  DATE          not null, -- Data da ultima atualizacao 
  Constraint pk1_d_idnBIEGE primary key (id)
)
  STORAGE 
(
  INITIAL 100 K 
  NEXT     20 K 
  MAXEXTENTS  5
  PCTINCREASE 0
);
--
Create table d_carBIEGE (
  ID_d_idnBIEGE NUMBER       NOT NULL, -- Identificador do calculo
  VL_LINHA      NUMBER(4,0)      NULL, -- Numero opcional da linha do grid
  DS_CARACT     VARCHAR2(10) NOT NULL, -- Descricao da caracteristica
  VL_CARACT     NUMBER(8,2)  NULL,     -- Valor da caracteristica
  Constraint pk1_d_carBIEGE primary key (ID_d_idnBIEGE,VL_LINHA,DS_CARACT),
  constraint fk1_d_carBIEGE foreign key (ID_d_idnBIEGE) references d_idnBIEGE(id)
)
  STORAGE 
(
  INITIAL  1000 K 
  NEXT     200 K 
  MAXEXTENTS  5
  PCTINCREASE 0
);
