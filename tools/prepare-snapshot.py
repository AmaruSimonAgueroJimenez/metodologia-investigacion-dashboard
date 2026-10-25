"""Build the reviewed teaching snapshot from a local extraction (no source edits)."""
import argparse
import copy
import datetime
import json
import re
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("input", type=Path)
parser.add_argument("output", type=Path)
parser.add_argument("--trace", type=Path)
args = parser.parse_args()
extraction = json.loads(args.input.read_text(encoding="utf-8"))
previous = json.loads(args.output.read_text(encoding="utf-8")) if args.output.exists() else {}
traces = {c["id"]: c for c in json.loads(args.trace.read_text())["cases"]} if args.trace else {}

DISPLAY = {
    "edad_anos": "Edad (años)", "distancia_al_cesfam_km": "Distancia al CESFAM (km)",
    "enferedad_cronica": "Enfermedad crónica", "frecuencia_de_controles_al_ano_n": "Controles al año (n)",
    "imc": "IMC", "cibercondria": "Cibercondría (puntaje CSS)",
    "gasto_en_farmacia_mensual": "Gasto mensual en farmacia ($)",
    "frecuancia_de_consumo_de_farmacos_dias_al_mes": "Consumo de fármacos (días al mes)",
    "ia_mas_utilizada": "IA más utilizada", "faramaco_mas_comun": "Fármaco más común",
    "anho_notificacion": "Año de notificación", "n_parejas_sexuales": "Número de parejas sexuales",
    "cie_10_diagnostico": "Diagnóstico CIE-10", "etapa_vih_sida": "Etapa VIH/SIDA",
    "clasificacion_final_vih": "Clasificación final VIH", "nombre_instruccion": "Nivel de instrucción",
    "dias_espera": "Días de espera", "distancia_km": "Distancia (km)", "n_consultas_urg": "Consultas de urgencia (n)",
    "priorizado": "Priorización médica", "sexo_biologico": "Sexo biológico", "macrozona": "Macrozona",
    "complica_previa": "Complicación previa", "gravedad_sintoma": "Gravedad del síntoma",
    "distancia_cesfam": "Distancia al CESFAM (km)", "n_consultas_tm": "Consultas de medicina tradicional (n)",
    "pertenece_pueblo": "Pertenece a pueblo originario", "inscri_pespi": "Inscripción PESPI",
    "medicina_prefer": "Medicina preferida", "satisfaccion_ces": "Satisfacción con la pertinencia cultural",
    "peso_kg": "Peso (kg)", "talla_cm": "Talla (cm)", "edad_gest_part": "Edad gestacional al parto (semanas)",
    "lactancia_mat": "Lactancia materna", "asiste_sala_cuna": "Asiste a sala cuna", "tipo_parto": "Tipo de parto",
    "cuidador_princ": "Cuidador principal", "eval_desarrollo": "Evaluación del desarrollo",
    "edad_inicio_sex": "Edad de inicio sexual (años)", "score_conocimien": "Conocimiento (puntaje)",
    "parejas_ult_ano": "Parejas en el último año (n)", "consejeria_prev": "Consejería previa",
    "metodo_principal": "Método principal", "frec_uso_condon": "Frecuencia de uso de condón",
    "imc_exacto": "IMC", "perimetro_cint": "Perímetro de cintura (cm)", "dias_activ_fis": "Actividad física (días por semana)",
    "diabetes_asoc": "Diabetes asociada", "sit_laboral": "Situación laboral", "tratamiento_act": "Tratamiento actual",
    "hba1c_valor": "HbA1c (%)", "creatinina_ser": "Creatinina sérica (mg/dL)", "anos_diagnost": "Años desde diagnóstico",
    "adherencia_far": "Adherencia farmacológica", "esquema_farma": "Esquema farmacológico",
    "complicacion_cr": "Complicación crónica", "riesgo_cv": "Riesgo cardiovascular", "anos_escolaridad": "Años de escolaridad",
    "n_gestiones_autonomas": "Gestiones autónomas (n)", "tipo_discap_visual": "Tipo de discapacidad visual",
    "n_controles_prenatales": "Controles prenatales (n)", "antec_preeclampsia": "Antecedente de preeclampsia",
    "control_1er_trimestre": "Control en primer trimestre", "edad_meses": "Edad (meses)",
    "peso_nacer_kg": "Peso al nacer (kg)", "n_hermanos": "Hermanos (n)", "n_meses_cuidado": "Tiempo de cuidado (meses)",
    "prevision_salud_pte": "Previsión de salud del paciente", "edad_gestacional_sem": "Edad gestacional (semanas)",
    "n_hijos_previos": "Hijos previos (n)",
}

# Codes define scale order when the source supplies a codebook. These textual
# scales have no numeric codebook; their explicit order preserves their meaning.
TEXT_SCALE_ORDER = {
    ("02", "estado_nutricional"): ["Normopeso", "Sobrepeso", "Obesidad"],
    ("03", "percepcion_del_paciente"): ["Muy Insatisfecho", "Insatisfecho", "Satisfecho", "Muy Satisfecho"],
    ("04", "rendimiento_academico_autorreporte"): ["MUY MALO", "MALO", "REGULAR", "BUENO", "MUY BUENO"],
    ("04", "sintomas_psicologicos_emocionales"): ["Mínimo", "Leve", "Moderado", "Severo"],
    ("05", "percepcion_de_riesgo"): ["Nulo", "Bajo", "Moderado", "Alto"],
    ("06.2", "uso_preservativo"): ["Nunca", "A veces", "Siempre", "No reporta"],
    ("07", "uso_preservativo"): ["Nunca", "Aveces", "Siempre"],
}

def category_values(item, column, rows):
    observed = list(dict.fromkeys(row[column["key"]] for row in rows if row[column["key"]] is not None))
    if not column.get("ordered"):
        return observed
    labels = column.get("valueLabels", {})
    if labels:
        # All current scale codes are integers. Keep unknown observed categories
        # after the documented scale instead of discarding them.
        order = [str(labels[key]).strip() for key in sorted(labels, key=lambda key: int(key))]
    else:
        order = TEXT_SCALE_ORDER.get((item["number"], column["key"]), [])
    return [value for value in order if value in observed] + [value for value in observed if value not in order]

def friendly(column):
    if column["key"] in DISPLAY:
        return DISPLAY[column["key"]]
    value = column["label"].replace("_", " ").lower()
    value = re.sub(r"\s+", " ", value).strip()
    for acronym in ["cesfam", "pespi", "vih", "sida", "imc", "ia"]:
        value = re.sub(r"\b" + acronym + r"\b", acronym.upper(), value)
    return value[:1].upper() + value[1:]

cases, queries = [], {}
now = datetime.datetime.now(datetime.timezone.utc).isoformat()
for original in extraction["cases"]:
    item = copy.deepcopy(original)
    rows = item.pop("rows")
    item["classification"] = "Datos simulados para docencia"
    item["classificationEvidence"] = "El responsable del curso confirmó el 25 de octubre de 2026 que todas las bases son completamente ficticias o simuladas."
    item["anonymization"] = "Se excluyeron los identificadores de registro. La publicación no contiene datos de alumnos, evaluaciones, asistencia ni soluciones docentes."
    item["notes"] = [n.replace("registros reales", "registros de datos") for n in item["notes"] if "No se verificó si los registros son reales o simulados" not in n]
    if item["number"] in ("06.1", "06.2", "07", "08"):
        item["notes"].insert(0, "El escenario menciona MINSAL, pero los datos se publican como simulados por confirmación del responsable del curso. No son estadísticas oficiales.")
    for column in item["columns"]:
        column["sourceOriginalLabel"] = column["label"]
        column["label"] = friendly(column)
        labels = column.get("valueLabels", {})
        if column["type"] == "categorical":
            for row in rows:
                value = row[column["key"]]
                if value is not None:
                    row[column["key"]] = str(labels.get(str(value), value)).strip()
            column["values"] = category_values(item, column, rows)
            column["distinct"] = len(column["values"])
    item["sourceSha256"] = traces.get(item["id"], {}).get("sha256")
    cases.append(item)
    queries[item["id"]] = {
        "rows": rows,
        "source": {
            "title": f"Caso {item['number']}: {item['title']}",
            "type": "file", "provider": "Archivos docentes locales", "classification": "synthetic",
            "files": [{"name": item["sourceFile"], "sheet": item["sheet"], "sha256": item["sourceSha256"]}],
            "description": f"{len(rows)} registros simulados. Una fila por observación docente. Identificadores excluidos.",
            "executedAt": now,
            "caveats": [item["classificationEvidence"], *item["notes"]],
            "evidenceFlow": [
                {"title": "Fuente", "detail": f"Versión para estudiantes: {item['sourceFile']}; hoja {item['sheet']}."},
                {"title": "Extracción", "detail": "Se leyeron encabezados y filas de la tabla, excluyendo identificadores, notas al pie, diccionarios laterales y filas vacías. En caso 09 se convirtieron comas decimales."},
                {"title": "Codificación", "detail": "Los códigos categóricos se tradujeron usando el diccionario del libro. Se preservaron categorías ordinales y valores numéricos. No se imputaron datos ni se corrigieron inconsistencias de la fuente."},
                {"title": "Naturaleza de los datos", "detail": item["classificationEvidence"]},
            ],
            "metricDefinitions": [
                {"label": "n", "definition": "Número de observaciones incluidas por los filtros de este caso. Las estadísticas numéricas y Pearson utilizan datos válidos o pares completos."},
                {"label": "Porcentaje", "definition": "Frecuencia / total de registros filtrados × 100. En cruces, el porcentaje fila usa el total de la fila y el total usa pares categóricos completos."},
                {"label": "Desviación estándar", "definition": "Desviación estándar muestral, con denominador n − 1. Indefinida con menos de dos valores."},
                {"label": "Cuartiles", "definition": "Interpolación lineal equivalente a quantile(..., type = 7) en R."},
                {"label": "Pearson r", "definition": "Correlación lineal entre pares numéricos completos. No se calcula con menos de tres pares ni con una variable constante."},
            ],
        },
        "methods": [{"language": "text", "code": "Filtrar filas por las categorías seleccionadas; calcular estadísticas en src/content/shared/statistics.js. Las tablas, los gráficos y las descargas usan esas mismas filas filtradas."}],
    }
queries["catalogo"] = {
    "rows": [{"case_id": c["id"], "caso": c["number"], "tema": c["title"], "registros": c["n"]} for c in cases],
    "source": {"title": "Catálogo de casos DSPU0011", "classification": "synthetic", "description": "20 bases docentes simuladas; la suma de registros combina escenarios independientes."},
}
snapshot = {
    "id": previous.get("id", "metodologia-casos-dspu0011"), "title": "Casos de investigación", "surface": "dashboard",
    "buildStatus": "creating", "generatedAt": now, "cases": cases, "queries": queries,
    "filters": [{"id": "caso", "label": "Caso", "field": "case_id", "defaultValue": "caso-01", "queryIds": ["catalogo"]}],
}
args.output.write_text(json.dumps(snapshot, ensure_ascii=False, allow_nan=False), encoding="utf-8")
print(json.dumps({"cases": len(cases), "records": sum(c["n"] for c in cases), "output": str(args.output)}, ensure_ascii=False))
