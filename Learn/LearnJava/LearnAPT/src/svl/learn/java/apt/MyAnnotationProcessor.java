package svl.learn.java.apt;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.*;
import javax.lang.model.util.Elements;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@SupportedAnnotationTypes("svl.learn.java.apt.NeedBuilderClass")
@SupportedSourceVersion(SourceVersion.RELEASE_8)
public class MyAnnotationProcessor  extends AbstractProcessor {

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        roundEnv.getElementsAnnotatedWith(NeedBuilderClass.class).stream().map(element -> (TypeElement) element).forEach((element1) -> {
            try {
                generate(element1);
            } catch (IOException e) {
                showError(e.getMessage());
            }
        });

        return true;
    }

    private void generate(TypeElement entityType) throws IOException {
        PackageElement packageElement = elementUtils().getPackageOf(entityType);
        Filer filer = processingEnv.getFiler();
        String builderName = entityType.getAnnotation(NeedBuilderClass.class).name();
        String entityName = entityType.getSimpleName().toString();

        JavaFileObject sourceFile = filer.createSourceFile(getQualifiedName(packageElement, builderName));
        List<Element> fields = getFields(entityType);

        showInfo("Generating: " + sourceFile.getName());

        try (PrintWriter printer = new PrintWriter(sourceFile.openWriter())) {
            printer.printf("package %s;\n\n", packageElement.getQualifiedName());

            printer.printf("public class %s{\n", builderName);

            fields.forEach(field -> printer.printf("\tprivate %s %s;\n", field.asType().toString(), field.getSimpleName()));

            fields.forEach(field -> {
                String dataType = field.asType().toString();
                String fieldName = field.getSimpleName().toString();
                String setMethodName = "set" + fieldName.toUpperCase().charAt(0) + fieldName.substring(1);

                printer.printf("\n");
                printer.printf("\tpublic %s %s(%s %s){\n", builderName, setMethodName, dataType, fieldName);
                printer.printf("\t\tthis.%s = %s;\n", fieldName, fieldName);
                printer.printf("\t\treturn this;\n");
                printer.printf("\t}\n");
            });

            List<List<? extends VariableElement>> constructors = getConstructors(entityType);
            if (constructors.isEmpty() || constructors.size() > 1) {
                showError(String.format("Entity %s class does not have constructor or has more than one constructor", entityName));
                return;
            }

            List<? extends VariableElement> constructor = constructors.get(0);
            StringBuilder buffer = new StringBuilder(128);
            constructor.forEach(el -> {
                buffer.append(el.getSimpleName()).append(", ");
            });

            printer.printf("\n");
            printer.printf("\tpublic %s build(){\n", entityName);
            printer.printf("\t\treturn new %s(%s);\n", entityName, buffer.toString().substring(0, buffer.length() - 2));
            printer.printf("\t}\n");


            printer.printf("}\n");
        }
    }

    private List<List<? extends VariableElement>> getConstructors(TypeElement entityType) {
        return elementUtils().getAllMembers(entityType).stream().
                filter(el -> el.getKind() == ElementKind.CONSTRUCTOR).
                map(element -> ((ExecutableElement) element).getParameters()).
                collect(Collectors.toList());
    }

    private List<Element> getFields(TypeElement element) {
        return elementUtils().getAllMembers(element).stream().
                filter(el -> el.getKind() == ElementKind.FIELD).
                collect(Collectors.toList());
    }

    private String getQualifiedName(PackageElement packageElement, String className) {
        return packageElement.getQualifiedName().toString() + "." + className;
    }

    private Elements elementUtils() {
        return processingEnv.getElementUtils();
    }

    @SuppressWarnings("unused")
    private void showMessage(String msg) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.WARNING, msg);
    }

    private void showInfo(String msg) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE, msg);
    }

    private void showError(String msg) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, msg);
    }
}