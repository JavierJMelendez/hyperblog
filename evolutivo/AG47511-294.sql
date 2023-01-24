ALTER TABLE creditocomercial.documentoschecklist ADD COLUMN aplicamultiple BOOLEAN DEFAULT FALSE;

COMMENT ON COLUMN creditocomercial.documentoschecklist.aplicamultiple
IS 'Si es true habilita al front para que permita subir multiples archivos para ese tipo de documento, si es false solo podrá subir 1 archivo por tipo de documento.';

COMMIT;

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

DROP FUNCTION IF EXISTS creditocomercial.gccconsultachecklist(json, json, json, json, json, json, boolean);

CREATE OR REPLACE FUNCTION creditocomercial.gccconsultachecklist(
	vtipobanca json,
	vtipogarantia json,
	vtipofacilidad json,
	vtiposolicitud json,
	vetapaproceso json,
	vtipofianza json,
	vclientegarante boolean)
    RETURNS TABLE(id integer, documento text, tipobanca character varying, tipogarantia character varying, tipofacilidad character varying, tiposolicitud character varying, exceptuado boolean, tipodocumental character varying, tiempovigenciadias integer, etapaproceso character varying, estado boolean, tipodocumentacion character varying, tipoexpediente character varying, idtipodocumental integer, tipofianza character varying, clientegarante boolean, esgeneral boolean, obligatorio boolean, aplicaobligatorio boolean, parametrostc character varying, aplicamultiple boolean) 
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE PARALLEL UNSAFE
    ROWS 1000

AS $BODY$
declare
i int;
vcount int;
vexiste integer;
BEGIN

--select * from creditocomercial.gccconsultachecklist('["PYME"]','[100]','["PCE"]','["NU"]','[4]','["SIPJ"]','true');

IF  vetapaproceso::jsonb @> '[0]'::jsonb THEN
	/* validacion de documentos obligatorios en presolicitud*/
    RETURN QUERY
            select a.id,
            a.documento,
            cast(a.tipobanca as varchar),
            cast(a.tipogarantia as varchar),
            cast(a.tipofacilidad as varchar),
            cast(a.tiposolicitud as varchar),
            a.exceptuado,
            a.tipodocumental,
            a.tiempovigenciadias,
            cast(a.etapaproceso as varchar),
            a.estado,
            a.tipodocumentacion,
            a.tipoexpediente,
            a.idtipodocumental,
            cast(a.tipofianza as varchar),
            a.clientegarante,
            a.esgeneral,
            a.obligatorio,
            a.aplicaobligatorio,
            coalesce(r.parametros,'') as parametrostc,
            a.aplicamultiple
        from creditocomercial.documentoschecklist a
        left join creditocomercial.tipodocumental r on a.idtipodocumental=r.idtipo
		where (a.etapaproceso->'etapaproceso')::jsonb @> '0'::jsonb
		  and a.obligatorio='true';

	RETURN;

ELSE
	/*validacion de documentos generales o sf */
	IF vtiposolicitud::jsonb @> '["NU"]'::jsonb and vetapaproceso::jsonb @> '[2]'::jsonb THEN

		CREATE TEMPORARY TABLE t1 ON COMMIT DROP AS
		(select a.id,
            a.documento,
            a.tipobanca,
            a.tipogarantia,
            a.tipofacilidad,
            a.tiposolicitud,
            a.exceptuado,
            a.tipodocumental,
            a.tiempovigenciadias,
            a.etapaproceso,
            a.estado,
            a.tipodocumentacion,
            a.tipoexpediente,
            a.idtipodocumental,
            a.tipofianza,
            a.clientegarante,
            a.esgeneral,
            a.obligatorio,
            a.aplicaobligatorio,
            coalesce(r.parametros,'') as parametrostc,
            a.aplicamultiple
			from creditocomercial.documentoschecklist a
			  left join creditocomercial.tipodocumental r on a.idtipodocumental=r.idtipo

            where (a.etapaproceso->'etapaproceso')::jsonb @> vetapaproceso::jsonb
			and a.estado='true'
			AND a.idseccionchk NOT IN ('SF'));

	ELSE

		CREATE TEMPORARY TABLE t1 ON COMMIT DROP AS
		(select *
			from creditocomercial.documentoschecklist a
		  where (a.etapaproceso->'etapaproceso')::jsonb @> vetapaproceso::jsonb
			and a.estado='TRUE');

	END IF;

CREATE TEMPORARY TABLE t2 ON COMMIT DROP AS
    select * from t1 a
     where (a.tipobanca->'tipobanca')::jsonb @> vtipobanca::jsonb;

CREATE TEMPORARY TABLE t3 ON COMMIT DROP AS
    select * from t1 a
     where (a.tipogarantia->'tipogarantia')::jsonb @> vtipogarantia::jsonb
        or length(a.tipogarantia::text) =19;

CREATE TEMPORARY TABLE t4 ON COMMIT DROP AS
    select * from t1 a
     where (a.tipofianza->'tipofianza')::jsonb @> vtipofianza::jsonb
        or  a.tipofianza is null;

CREATE TEMPORARY TABLE t5 ON COMMIT DROP AS
    select * from t1 a
     where (a.tipofacilidad->'tipofacilidad')::jsonb @> vtipofacilidad::jsonb
        or length(a.tipofacilidad::text) =20;

CREATE TEMPORARY TABLE t6 ON COMMIT DROP AS
    select * from t1 a
     where (a.tiposolicitud->'tiposolicitud')::jsonb @> vtiposolicitud::jsonb
     or length(a.tiposolicitud::text) < 24;

CREATE TEMPORARY TABLE t7 ON COMMIT DROP AS
    select * from t1 a
     where (a.tiposolicitud->'tiposolicitud')::jsonb @> vtiposolicitud::jsonb
     or length(a.tiposolicitud::text) < 24;

CREATE TEMPORARY TABLE t8 ON COMMIT DROP AS
    select * from t1 a
     where a.clientegarante = vclientegarante
        or  a.clientegarante is null;

    RETURN QUERY
            select a.id,
            a.documento,
            cast(a.tipobanca as varchar),
            cast(a.tipogarantia as varchar),
            cast(a.tipofacilidad as varchar),
            cast(a.tiposolicitud as varchar),
            a.exceptuado,
            a.tipodocumental,
            a.tiempovigenciadias,
            cast(a.etapaproceso as varchar),
            a.estado,
            a.tipodocumentacion,
            a.tipoexpediente,
            a.idtipodocumental,
            cast(a.tipofianza as varchar),
            a.clientegarante,
            a.esgeneral,
            a.obligatorio,
            a.aplicaobligatorio,
            a.parametrostc,
            a.aplicamultiple
        from t2 a
       join t7 f on f.id = a.id
       join t4 b on b.id = f.id
       join t3 c on c.id = b.id
       join t5 d on d.id = c.id
       join t6 e on e.id = d.id
       join t8 h on h.id = f.id
       order by a.documento;
RETURN;

END IF;

END;
$BODY$;

ALTER FUNCTION creditocomercial.gccconsultachecklist(json, json, json, json, json, json, boolean)
    OWNER TO bpm;

COMMIT;

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

ALTER TABLE creditocomercial.checklisttramite ADD COLUMN clientdocumentid character varying DEFAULT '';

COMMENT ON COLUMN creditocomercial.checklisttramite.clientdocumentid
IS 'Se usa para identificar al participante que le pertenece el documento, solamente se llena para documentos que aplicamultiple es true.';

ALTER TABLE creditocomercial.checklisttramite ADD COLUMN generationmethod character varying(1) DEFAULT 'M';

COMMENT ON COLUMN creditocomercial.checklisttramite.generationmethod
IS 'Si es M fue creado manualmente por el usuario, si es O se creó automaticamente usando OnBase.';

ALTER TABLE creditocomercial.checklisttramite DROP CONSTRAINT checklisttramite_pkey;

COMMIT;

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- Para Borrar la función antigua cuando se deje de usar.

-- DROP FUNCTION IF EXISTS creditocomercial.gccactualizarchecklisttramite(integer, integer, integer, text, character varying, boolean, date, text, boolean, date, boolean, integer, boolean);

CREATE OR REPLACE FUNCTION creditocomercial.gccactualizarchecklisttramite(
	vidtramite integer,
	vidpersona integer,
	vidchecklist integer,
	vdocumentochk text,
	vidonbase character varying,
	vestado boolean,
	vfechavencimiento date,
	vcomentarios text,
	vexceptuado boolean,
	vfechacompromiso date,
	vrecibidoca boolean,
	vidfacilidad integer,
	vaplicaobligatorio boolean,
	vclientdocumentid character varying,
	vgenerationmethod character varying,
	OUT v_message character varying,
	OUT v_end_code character varying)
    RETURNS record
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE PARALLEL UNSAFE
AS $BODY$
DECLARE
	SQL TEXT;
BEGIN

	IF NOT EXISTS (SELECT FROM creditocomercial.checklisttramite a
		WHERE a.idtramite = vidtramite
		AND a.idpersona = vidpersona
		AND a.idchecklist = vidchecklist
		AND a.idfacilidad = vidfacilidad
		AND a.clientdocumentid = vclientdocumentid
		AND a.estado = true) THEN
		v_message := 'registro no existente';
		v_end_code := '10001';
		RETURN;
	END IF;

	IF vgenerationmethod = 'M' THEN
		UPDATE creditocomercial.checklisttramite a
		SET fecha = CURRENT_DATE,
			idonbase = vidonbase,
			estado = vestado,
			fechavencimiento = vfechavencimiento,
			exceptuado = vexceptuado,
			fechacompromiso = vfechacompromiso,
			comentarios = vcomentarios,
			recibidoca = vrecibidoca,
			aplicaobligatorio = vaplicaobligatorio,
			generationmethod = vgenerationmethod
		WHERE a.idtramite = vidtramite
			AND a.idpersona = vidpersona
			AND a.idchecklist = vidchecklist
			AND a.idfacilidad = vidfacilidad
			AND a.clientdocumentid = vclientdocumentid
			AND a.estado = true;
	END IF;

	v_message := 'operacion exitosa';
	v_end_code := '00000';
	RETURN;

END;
$BODY$;

ALTER FUNCTION creditocomercial.gccactualizarchecklisttramite(integer, integer, integer, text, character varying, boolean, date, text, boolean, date, boolean, integer, boolean, character varying, character varying)
    OWNER TO bpm;

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

-- Para Borrar la función antigua cuando se deje de usar.

-- DROP FUNCTION IF EXISTS creditocomercial.gccguardarchecklisttramite(integer, integer, integer, text, character varying, boolean, date, text, boolean, date, boolean, integer, boolean);

CREATE OR REPLACE FUNCTION creditocomercial.gccguardarchecklisttramite(
	vidtramite integer,
	vidpersona integer,
	vidchecklist integer,
	vdocumentochk text,
	vidonbase character varying,
	vestado boolean,
	vfechavencimiento date,
	vcomentarios text,
	vexceptuado boolean,
	vfechacompromiso date,
	vrecibidoca boolean,
	vidfacilidad integer,
	vaplicaobligatorio boolean,
	vclientdocumentid character varying,
	vgenerationmethod character varying,
	OUT v_message character varying,
	OUT v_end_code character varying)
    RETURNS record
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE PARALLEL UNSAFE
AS $BODY$
DECLARE
	SQL TEXT;
BEGIN
	IF EXISTS (SELECT FROM creditocomercial.checklisttramite a
		  WHERE a.idtramite=vidtramite
			AND a.idpersona = vidpersona
			AND a.idchecklist = vidchecklist
			AND a.idfacilidad= vidfacilidad
			and a.clientdocumentid=vclientdocumentid
			AND a.estado='true') THEN
		v_message := 'documento registrado para este tramite/persona hacer update';
		v_end_code := '10001';
	ELSE 
		INSERT INTO creditocomercial.checklisttramite(idtramite, idpersona, idchecklist, documentochk,fecha, idonbase, estado,fechavencimiento,comentarios, exceptuado,
													  fechacompromiso,recibidoca,idfacilidad,aplicaobligatorio,clientdocumentid,generationmethod)
		VALUES (vidtramite, vidpersona, vidchecklist, vdocumentochk,CURRENT_DATE, vidonbase, 'TRUE',vfechavencimiento,vcomentarios, vexceptuado,
				vfechacompromiso,vrecibidoca,vidfacilidad,vaplicaobligatorio,vclientdocumentid,vgenerationmethod);
		v_message := 'operacion exitosa';
		v_end_code := '00000';
		RETURN;		
	END IF;
END;
$BODY$;

ALTER FUNCTION creditocomercial.gccguardarchecklisttramite(integer, integer, integer, text, character varying, boolean, date, text, boolean, date, boolean, integer, boolean, character varying, character varying)
    OWNER TO bpm;

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION creditocomercial.gccconsultarchecklisttramite(
	vidtramite integer,
	vidpersona integer,
	vidchecklist integer)
    RETURNS TABLE(idtramite integer, idpersona integer, idchecklist integer, documentochk text, fecha date, idonbase character varying, estado boolean, fechavencimiento date, comentarios text, exceptuado boolean, fechacompromiso date, recibidoca boolean, idfacilidad integer, aplicaobligatorio boolean, clientdocumentid character varying, generationmethod character varying) 
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE PARALLEL UNSAFE
    ROWS 1000

AS $BODY$
DECLARE
SQL TEXT;
BEGIN
	IF vidtramite is not null then
		IF vidpersona <> 0 then
			RETURN QUERY
			SELECT * FROM creditocomercial.checklisttramite c WHERE c.idtramite = vidtramite AND c.idpersona=vidpersona AND c.estado='true';
			return;
		ELSIF vidchecklist <> 0 then
			RETURN QUERY
			SELECT * FROM creditocomercial.checklisttramite c WHERE c.idtramite = vidtramite AND c.idchecklist = vidchecklist AND c.estado='true';
			return;
		END if;
	END if;
END;
$BODY$;

ALTER FUNCTION creditocomercial.gccconsultarchecklisttramite(integer, integer, integer)
    OWNER TO bpm;

------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------

CREATE OR REPLACE FUNCTION creditocomercial.gcceliminarchecklisttramite(
	vidtramite integer,
	vidpersona integer,
	vidchecklist integer,
	vidfacilidad integer,
	vclientdocumentid character varying,
	OUT v_message character varying,
	OUT v_end_code character varying)
    RETURNS record
    LANGUAGE 'plpgsql'
    COST 100
    VOLATILE PARALLEL UNSAFE
AS $BODY$
DECLARE
SQL TEXT;
BEGIN
	
   IF EXISTS (SELECT FROM creditocomercial.checklisttramite a
              WHERE a.idtramite=vidtramite
			    AND a.idpersona= vidpersona
				and a.idchecklist=vidchecklist
                and a.idfacilidad=vidfacilidad
			  	and a.clientdocumentid=vclientdocumentid
			 	AND a.estado='true') THEN
				
			UPDATE creditocomercial.checklisttramite a
				SET estado=false
			  WHERE a.idtramite=vidtramite
			    AND a.idpersona= vidpersona
			    and a.idfacilidad=vidfacilidad
				and a.idchecklist=vidchecklist
				and a.clientdocumentid=vclientdocumentid
			 	AND a.estado='true';

			v_message := 'operacion exitosa';
			v_end_code := '00000';
		
		RETURN;

   ELSE
		v_message := 'registro no existente';
		v_end_code := '10001';
		RETURN;
    END IF;	

END;
$BODY$;

ALTER FUNCTION creditocomercial.gcceliminarchecklisttramite(integer, integer, integer, integer, character varying)
    OWNER TO bpm;
