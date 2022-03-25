package iesb.edu.br.turismo;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import java.util.List;

import iesb.edu.br.turismo.tratadados.MonumentoDTO;

public class CustomAdapter extends ArrayAdapter {

    private List<MonumentoDTO> items;
    private Context c;

    public CustomAdapter(Context context, int resource, List<MonumentoDTO> objects) {
        super(context, resource, objects);
        this.c = context;
        this.items = objects;
    }

    @Override
    public int getCount() {
        return super.getCount();
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            LayoutInflater inflater = (LayoutInflater)getContext().getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            v = inflater.inflate(R.layout.activity_detalhe, null);
        }

        // Realizar as associações dos componentes do Layout
        ImageView imageView = v.findViewById(R.id.idImagem);
        TextView textView = v.findViewById(R.id.idDescricao);

        MonumentoDTO item = items.get(position);
        // Carregar a Imagem
        int id = c.getResources().getIdentifier("drawable/"+item.getNomeImg(), null, c.getPackageName());
        imageView.setImageResource(id);
        // Carregar a Descrição
        textView.setText(item.getDescricao());
        return v;
    }
}